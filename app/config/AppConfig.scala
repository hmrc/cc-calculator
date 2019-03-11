/*
 * Copyright 2019 HM Revenue & Customs
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

package config

import javax.inject.Inject
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class AppConfig @Inject()(val config: ServicesConfig) {

  val defaultMaxNoOfChildren = 25
  val defaultMaxNameLength = 25

  lazy val upperMonthsLimitValidation: Int = config.getInt("esc.months-upper-limit")
  lazy val lowerMonthsLimitValidation: Int = config.getInt("esc.months-lower-limit")
  lazy val lowerPeriodsLimitValidation: Int = config.getInt("esc.periods-lower-limit")
  lazy val lowerTaxYearsLimitValidation: Int = config.getInt("esc.tax-years-lower-limit")
  lazy val lowerClaimantsLimitValidation: Int =  config.getInt("esc.claimants-lower-limit")

  lazy val maxNoOfChildren: Int = config.getInt("tfc.max-no-of-children")
  lazy val maxNameLength: Int = config.getInt("tfc.max-name-length")
  lazy val taxYearEndMonth: Int = config.getInt("tfc.end-of-tax-year-date.month")
  lazy val taxYearEndDay: Int = config.getInt("tfc.end-of-tax-year-date.day")

  def schemeMonth(schemeName: String): Int = config.getInt(s"$schemeName.end-of-tax-year-date.month")
  def schemeDay(schemeName: String): Int = config.getInt(s"$schemeName.end-of-tax-year-date.day")
}


