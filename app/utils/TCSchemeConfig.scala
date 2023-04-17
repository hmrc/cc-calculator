/*
 * Copyright 2023 HM Revenue & Customs
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

import config.AppConfig
import javax.inject.Inject
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.Configuration

case class WTC(
                basicElement : Int,
                coupleElement : Int,
                loneParentElement: Int,
                hours30Element: Int,
                disabledWorkerElement: Int,
                severeDisabilityWorkerElement: Int,
                maxChildcareOneChildElement: Int,
                maxChildcareMoreChildrenElement: Int,
                eligibleCostCoveredPercent: Int
                )

case class CTC(
                childElement: Int,
                youngPersonElement: Int,
                disabledChildElement: Int,
                severeDisabilityChildElement: Int,
                familyElement: Int
                )

case class Thresholds(
                       wtcIncomeThreshold: Int,
                       ctcIncomeThreshold: Int,
                       taperRatePercent: Int
                       )

case class TCTaxYearConfig(
                            otherIncomeAdjustment: Double,
                            currentIncomeFallDifferenceAmount: Double,
                            currentIncomeRiseDifferenceAmount: Double,
                            wtc: WTC,
                            ctc: CTC,
                            thresholds: Thresholds
                            )


class TCConfig @Inject()(config: AppConfig,
                         configuration: Configuration) extends CCConfig(config) {

  lazy val monthsInTaxYear: Int = 12

  def getCurrentTaxYearDateRange(fromDate : LocalDate) : (LocalDate, LocalDate) = {
    val pattern = "dd-MM-yyyy"
    val formatter = DateTimeFormat.forPattern(pattern)
    val month = config.taxYearEndMonth
    val day = config.taxYearEndDay
    val currentTaxYear = getCurrentTaxYear(fromDate)
    val taxYearStartDate = LocalDate.parse(s"$day-$month-$currentTaxYear", formatter)
    val taxYearEndDate = LocalDate.parse(s"$day-$month-${currentTaxYear + 1}", formatter)
    (taxYearStartDate, taxYearEndDate)
  }

  def getConfig(currentDate: LocalDate) : TCTaxYearConfig  = {

    val configs: Seq[play.api.Configuration] = configuration.get[Seq[Configuration]]("tc.rule-change")
    // get the default config and keep
    val defaultConfig = configs.find(_.get[String]("rule-date").contains("default")).head
    // fetch the config if it matches the particular year
    val result = getConfigForTaxYear(currentDate, configs)

    val config : TCTaxYearConfig = result match {
      case Some(x) =>
        getTaxYear(x)
      case _ =>
        getTaxYear(defaultConfig)
    }
    config
  }

  def getTaxYear(config : Configuration): TCTaxYearConfig = {
    TCTaxYearConfig(
      otherIncomeAdjustment = config.get[Double]("other-adjustment"),
      currentIncomeFallDifferenceAmount = config.get[Double]("current-income-fall-difference-amount"),
      currentIncomeRiseDifferenceAmount = config.get[Double]("current-income-rise-difference-amount"),
      wtc = WTC(
        basicElement = config.get[Int]("input-elements.wtc.basic-element"),
        coupleElement = config.get[Int]("input-elements.wtc.second-adult-element"),
        loneParentElement = config.get[Int]("input-elements.wtc.lone-parent-element"),
        hours30Element = config.get[Int]("input-elements.wtc.30-hour-element"),
        disabledWorkerElement = config.get[Int]("input-elements.wtc.disabled-worker-element"),
        severeDisabilityWorkerElement = config.get[Int]("input-elements.wtc.severe-disabled-worker-element"),
        maxChildcareOneChildElement = config.get[Int]("input-elements.wtc.max-childcare-element-one-child"),
        maxChildcareMoreChildrenElement = config.get[Int]("input-elements.wtc.max-childcare-element-more-children"),
        eligibleCostCoveredPercent = config.get[Int]("input-elements.wtc.percent-of-eligible-cost-covered")
      ),
      ctc = CTC(
        childElement = config.get[Int]("input-elements.ctc.child-element"),
        youngPersonElement = config.get[Int]("input-elements.ctc.young-person-element"),
        disabledChildElement = config.get[Int]("input-elements.ctc.disabled-child-element"),
        severeDisabilityChildElement = config.get[Int]("input-elements.ctc.severe-disabled-child-element"),
        familyElement = config.get[Int]("input-elements.ctc.family-element")
      ),
      thresholds = Thresholds(
        wtcIncomeThreshold = config.get[Int]("thresholds.wtc-income-threshold"),
        ctcIncomeThreshold = config.get[Int]("thresholds.ctc-income-threshold"),
        taperRatePercent = config.get[Int]("thresholds.percent-of-taper-rate")
      )
    )
  }
}
