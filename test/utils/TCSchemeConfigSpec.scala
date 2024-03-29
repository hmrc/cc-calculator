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

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table

class TCSchemeConfigSpec extends FakeCCCalculatorApplication with MockitoSugar {

  override val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")

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
      disabledWorkerElement = 3090,
      severeDisabilityWorkerElement = 1330,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3275,
      severeDisabilityChildElement = 1325,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2019 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 1960,
      coupleElement = 2010,
      loneParentElement = 2010,
      hours30Element = 810,
      disabledWorkerElement = 3090,
      severeDisabilityWorkerElement = 1330,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2780,
      childElement = 2780,
      disabledChildElement = 3275,
      severeDisabilityChildElement = 1325,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6420,
      ctcIncomeThreshold = 16105,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2022 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 2070,
      coupleElement = 2125,
      loneParentElement = 2125,
      hours30Element = 860,
      disabledWorkerElement = 3345,
      severeDisabilityWorkerElement = 1445,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 2935,
      childElement = 2935,
      disabledChildElement = 3545,
      severeDisabilityChildElement = 1430,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 6770,
      ctcIncomeThreshold = 17005,
      taperRatePercent = 41
    )
  )

  val taxYearConfig2023 = TCTaxYearConfig(
    otherIncomeAdjustment = 300,
    currentIncomeFallDifferenceAmount = 2500,
    currentIncomeRiseDifferenceAmount = 2500,
    wtc = WTC(
      basicElement = 2280,
      coupleElement = 2340,
      loneParentElement = 2340,
      hours30Element = 950,
      disabledWorkerElement = 3685,
      severeDisabilityWorkerElement = 1595,
      maxChildcareOneChildElement = 175,
      maxChildcareMoreChildrenElement = 300,
      eligibleCostCoveredPercent = 70
    ),
    ctc = CTC(
      youngPersonElement = 3235,
      childElement = 3235,
      disabledChildElement = 3905,
      severeDisabilityChildElement = 1575,
      familyElement = 545
    ),
    thresholds = Thresholds(
      wtcIncomeThreshold = 7455,
      ctcIncomeThreshold = 18725,
      taperRatePercent = 41
    )
  )


  "TC SchemeConfig" must {
    val tcConfig = app.injector.instanceOf[TCConfig]
    "return 12 for months in tax year" in {
      tcConfig.monthsInTaxYear shouldBe 12
    }

    "Tax year end month from config file" in {
      tcConfig.appConfig.taxYearEndMonth shouldBe 4
    }

    "Tax year end date from config file" in {
      tcConfig.appConfig.taxYearEndDay shouldBe 6
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
      ("2018", "23-07-2018", taxYearConfig2018),
      ("2019", "23-07-2019", taxYearConfig2019),
      ("2022", "23-07-2022", taxYearConfig2022),
      ("2023", "23-07-2023", taxYearConfig2023)
    )

    forAll(configTestCases) { case (taxYear, date, taxYearConfig) =>

      s"return $taxYear TC taxYear Config for a date $date" in {
        val fromDate = LocalDate.parse(date, formatter)
        val tcConfig = app.injector.instanceOf[TCConfig]
        val config = tcConfig.getConfig(fromDate)
        config shouldBe taxYearConfig
      }
    }

    val datesTestCases = Table(
      ("test date", "ty start date", "ty end date"),
      ("01-01-2017", "06-04-2016", "06-04-2017"),
      ("06-04-2017", "06-04-2017", "06-04-2018"),
      ("05-04-2017", "06-04-2016", "06-04-2017"),
      ("10-04-2023", "06-04-2023", "06-04-2024")
    )

    forAll(datesTestCases) { case (testDate, startDate, endDate) =>

      s"return tax year start date: $startDate and end date $endDate when passed date is $testDate" in {
        val today = LocalDate.parse(testDate, formatter)
        val taxYearStartDate = LocalDate.parse(startDate, formatter)
        val taxYearEndDate = LocalDate.parse(endDate, formatter)
        val tcConfig = app.injector.instanceOf[TCConfig]
        val resultTuple = tcConfig.getCurrentTaxYearDateRange(today)
        resultTuple._1 shouldBe taxYearStartDate
        resultTuple._2 shouldBe taxYearEndDate
      }
    }
  }
}
