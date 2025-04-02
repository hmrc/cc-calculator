/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package utils

import java.text.SimpleDateFormat
import java.util.Calendar

import config.AppConfig
import javax.inject.Inject
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Date
import java.time.ZoneId.systemDefault
import play.api.Configuration

class CCConfig @Inject()(val appConfig: AppConfig) {

  val formatterDatePattern = "dd-MM-yyyy"

  def getConfigForTaxYear(currentDate: LocalDate, configs : Seq[Configuration]) : Option[Configuration] =  {
    //get the configs for all years except the default
    val configsExcludingDefault =
      configs.filterNot(_.get[String]("rule-date").contains("default"))

    // ensure the latest date is in the head position
    val sorted = configsExcludingDefault.sortBy(c => {
      val predicate = new SimpleDateFormat(formatterDatePattern).parse(c.get[String]("rule-date"))
      predicate
    }).reverse
    val result = getConfigHelper(currentDate, sorted.toList, None)
    result
  }

  def getConfigHelper(currentDate: LocalDate, taxYearConfigs: List[Configuration], acc: Option[Configuration]): Option[Configuration] = {

    taxYearConfigs match {
      case Nil => acc
      case head :: tail =>
        val configDate = new SimpleDateFormat(formatterDatePattern).parse(head.get[String]("rule-date"))
        // exit tail recursive
        if (toDate(currentDate).after(configDate) || toDate(currentDate).compareTo(configDate) == 0) {
          getConfigHelper(currentDate, Nil, Some(head))
        } else {
          getConfigHelper(currentDate, tail, acc)
        }
    }
  }

  def taxYearEndDate(now: LocalDate = LocalDate.now(), schemeName: String): LocalDate = {
    val month = appConfig.schemeMonth(schemeName)
    val day = appConfig.schemeDay(schemeName)
    // have to determine the year as this is not a fixed date
    val year = {
      val calendar = Calendar.getInstance()
      val currentYear = now.getYear
      calendar.set(currentYear, month, day)
      val taxDate = calendar.getTime
      if (taxDate.before(toDate(now))) {
        calendar.add(Calendar.YEAR, 1)
      }
      calendar.get(Calendar.YEAR)
    }
    val dayPattern = if (day.toString.length == 1) "d" else "dd"
    val monthPattern = if (month.toString.length == 1) "M" else "MM"
    val formatter = DateTimeFormatter.ofPattern(s"$dayPattern-$monthPattern-yyyy")
    LocalDate.parse(s"$day-$month-$year", formatter)
  }

  private def toDate(localDate: LocalDate): Date = {
    Date.from(localDate.atStartOfDay(systemDefault).toInstant)
  }
}
