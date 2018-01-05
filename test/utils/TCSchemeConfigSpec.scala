/*
 * Copyright 2018 HM Revenue & Customs
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

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class TCSchemeConfigSpec extends FakeCCCalculatorApplication {

  override val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")

  val defaultTaxYearConfig = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 1960,
      coupleElement = 2010,
      loneParentElement = 2010,
      hours30Element = 810,
      disabledWorkerElement = 3000,
      severeDisabilityWorkerElement = 1290,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3175,
      severeDisabilityChildElement = 1290,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2016 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 1960,
      coupleElement = 2010,
      loneParentElement = 2010,
      hours30Element = 810,
      disabledWorkerElement =  2970,
      severeDisabilityWorkerElement = 1275,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3140,
      severeDisabilityChildElement = 1275,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2017 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 1960,
      coupleElement = 2010,
      loneParentElement = 2010,
      hours30Element = 810,
      disabledWorkerElement = 3000,
      severeDisabilityWorkerElement = 1290,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3175,
      severeDisabilityChildElement = 1290,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2018 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 1960,
      coupleElement = 2010,
      loneParentElement = 2010,
      hours30Element = 810,
      disabledWorkerElement = 3000,
      severeDisabilityWorkerElement = 1290,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3175,
      severeDisabilityChildElement = 1290,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  "TC SchemeConfig" should {

    "return 12 for months in tax year" in {
      val tcConfig = new TCConfig {}
      tcConfig.monthsInTaxYear shouldBe 12
    }

    "Tax year end month from config file" in {
      val tcConfig = new TCConfig {}
      tcConfig.taxYearEndMonth shouldBe 4
    }

    "Tax year end date from config file" in {
      val tcConfig = new TCConfig {}
      tcConfig.taxYearEndDay shouldBe 6
    }

    val configTestCases = Table(
      ("taxYear", "date", "taxYearConfig"),
      ("default", "07-07-2014", defaultTaxYearConfig),
      ("default", "05-04-2016", defaultTaxYearConfig),
      ("2016", "23-07-2016", taxYearConfig2016),
      ("2016", "07-01-2017", taxYearConfig2016),
      ("2017", "23-07-2017", taxYearConfig2017),
      ("2017", "06-04-2017", taxYearConfig2017),
      ("2017", "06-01-2018", taxYearConfig2017),
      ("2017", "05-04-2018", taxYearConfig2017),
      ("2018", "06-04-2018", taxYearConfig2018),
      ("2018", "23-07-2018", taxYearConfig2018)
    )

    forAll(configTestCases) { case (taxYear, date, taxYearConfig) =>

      s"return ${taxYear} TC taxYear Config for a date ${date}" in {
        val fromDate = LocalDate.parse(date, formatter)
        val tcConfig = new TCConfig {}
        val config = tcConfig.getConfig(fromDate)
        config shouldBe taxYearConfig
      }

    }

    val datesTestCases = Table(
      ("test date", "ty start date", "ty end date"),
      ("01-01-2017", "06-04-2016", "06-04-2017"),
      ("06-04-2017", "06-04-2017", "06-04-2018"),
      ("05-04-2017", "06-04-2016", "06-04-2017")
    )

    forAll(datesTestCases) { case (testDate, startDate, endDate) =>

      s"return tax year start date: ${startDate} and end date ${endDate} when passed date is ${testDate}" in {
        val today = LocalDate.parse(testDate, formatter)
        val taxYearStartDate = LocalDate.parse(startDate, formatter)
        val taxYearEndDate = LocalDate.parse(endDate, formatter)
        val tcConfig = new TCConfig {}
        val resultTuple = tcConfig.getCurrentTaxYearDateRange(today)
        resultTuple._1 shouldBe taxYearStartDate
        resultTuple._2 shouldBe taxYearEndDate
      }

    }
  }
}
