/*
 * Copyright 2017 HM Revenue & Customs
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

/**
 * Created by ben on 27/01/16.
 */
class TCSchemeConfigSpec extends FakeCCCalculatorApplication with TCConfig {

  "TC SchemeConfig" should {

    "max no of characters of childrens name from config file" in {
      TCConfig.maxNameLength shouldBe 25
    }

    "Tax year end month from config file" in {
      TCConfig.taxYearEndMonth shouldBe 4
    }

    "Tax year end date from config file" in {
      TCConfig.taxYearEndDay shouldBe 6
    }

    "return Default TC taxYear Config for a date 23-07-2016" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-07-2016", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return 2017 TC taxYear Config for a date 23-07-2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-07-2017", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return Default TC taxYear Config for a date 07-07-2014" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("07-07-2014", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear

    }

    "return Default TC taxYear Config for a date 05-04-2016" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("05-04-2016", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return 2017 TC taxYear Config - 06-04-2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-04-2017", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return 2017 TC taxYear Config - 06-01-2018" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-01-2018", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return Default TC taxYear Config for a date 07-01-2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("07-01-2017", formatter)
      val config = TCConfig.getConfig(fromDate)
      val tcTaxYear = TCTaxYearConfig(
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
      config shouldBe tcTaxYear
    }

    "return tax year start and end dates for tax year 2016/2017 when a date is passed (date in calendar year 2017)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2017-01-01", formatter)
      val taxYearStartDate = LocalDate.parse("2016-04-06", formatter)
      val taxYearEndDate = LocalDate.parse("2017-04-06", formatter)

      val resultTuple = TCConfig.getCurrentTaxYearDateRange(today)
      resultTuple._1 shouldBe taxYearStartDate
      resultTuple._2 shouldBe taxYearEndDate
    }

    "return tax year start and end dates for tax year 2017/2018 when a date is passed (date in calendar year 2017)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2017-04-06", formatter)
      val taxYearStartDate = LocalDate.parse("2017-04-06", formatter)
      val taxYearEndDate = LocalDate.parse("2018-04-06", formatter)

      val resultTuple = TCConfig.getCurrentTaxYearDateRange(today)
      resultTuple._1 shouldBe taxYearStartDate
      resultTuple._2 shouldBe taxYearEndDate
    }

    "return tax year start and end dates for tax year 2016/2017 when a date is passed (date in calendar year 2017, one day before the end of tax year)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val today = LocalDate.parse("2017-04-05", formatter)
      val taxYearStartDate = LocalDate.parse("2016-04-06", formatter)
      val taxYearEndDate = LocalDate.parse("2017-04-06", formatter)

      val resultTuple = TCConfig.getCurrentTaxYearDateRange(today)
      resultTuple._1 shouldBe taxYearStartDate
      resultTuple._2 shouldBe taxYearEndDate
    }
  }
}
