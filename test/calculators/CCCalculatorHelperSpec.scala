/*
 * Copyright 2021 HM Revenue & Customs
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

package calculators

import org.joda.time.LocalDate
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, Periods}

class CCCalculatorHelperSpec extends UnitSpec with FakeCCCalculatorApplication {

  object helper extends CCCalculatorHelper

  "CCCalculatorHelper" should {

    "round up for double numbers to 2 digits" in {
      val cost: BigDecimal = 12.249212342
      val result: BigDecimal = helper.round(cost)
      result shouldBe 12.25
      result.isInstanceOf[BigDecimal] shouldBe true
    }

    "round down for double numbers to 2 digits" in {
      val cost: BigDecimal = 12.241212342
      val result: BigDecimal = helper.round(cost)
      result shouldBe 12.24
    }

    "(Pound precision) round up for double numbers" in {
      val cost: BigDecimal = 12.649212342
      val result: BigDecimal = helper.roundToPound(cost)
      result shouldBe 13
    }

    "(Pound precision) round down for double numbers" in {
      val cost : BigDecimal = 12.241212342
      val result : BigDecimal = helper.roundToPound(cost)
      result shouldBe 12
    }

    "round up the double numbers to 2 digits for digits less than 5" in {
      val cost: BigDecimal = 12.241212342
      val result: BigDecimal = helper.roundup(cost)
      result shouldBe 12.25
    }

    "round up the double numbers to 2 digits for digits greater than 5" in {
      val cost: BigDecimal = 12.24932342
      val result: BigDecimal = helper.roundup(cost)
      result shouldBe 12.25
    }

    "round down the double numbers to 3 digits upper digits" in {
      val cost: BigDecimal = 12.24932342
      val result: BigDecimal = helper.roundDownToThreeDigits(cost)
      result shouldBe 12.249
    }

    "convert the amount to a quarterly amount (weekly)" in {
      val cost : BigDecimal = 200.00 // per week
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Weekly)
      result shouldBe 2600.00
    }

    "convert the amount to a quarterly amount (monthly)" in {
      val cost : BigDecimal = 200.00 // per month
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Monthly)
      result shouldBe 600.00
    }

    "convert the amount to a quarterly amount (yearly)" in {
      val cost : BigDecimal = 200.00 // per year
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Yearly)
      result shouldBe 50.00
    }

    "return 0 if invalid period is given" in {
      val result : BigDecimal = helper.amountToQuarterlyAmount(200, Periods.INVALID)
      result shouldBe 0
    }

    "return the number of days between two dates" in {
      val fromDate = LocalDate.now()
      val toDate = fromDate.plusDays(11)
      val result: Int = helper.daysBetween(fromDate, toDate)
      result shouldBe 11
    }

    "amountToMonthlyAmount" should {
      "convert weekly amount to monthly" in {
        val result = helper.amountToMonthlyAmount(120, Periods.Weekly)
        result shouldBe 520
      }

      "convert monthly amount to monthly" in {
        val result = helper.amountToMonthlyAmount(520, Periods.Monthly)
        result shouldBe 520
      }

      "convert yearly amount to monthly" in {
        val result = helper.amountToMonthlyAmount(6240, Periods.Yearly)
        result shouldBe 520
      }

      "return 0 if invalid period is given" in {
        val result = helper.amountToMonthlyAmount(6240, Periods.INVALID)
        result shouldBe 0
      }
    }

    "amountFromPeriodToDaily" should {
      "return 0 if invalid period is given" in {
        val result = helper.amountFromPeriodToDaily(6240, Periods.INVALID, 365)
        result shouldBe 0
      }

      "return daily amount if monthly one is given" in {
        val result = helper.amountFromPeriodToDaily(365, Periods.Monthly, 365)
        result shouldBe 12
      }
    }

    "amountToWeeklyAmount" should {
      "convert weekly amount to weekly" in {
        val result = helper.amountToWeeklyAmount(120, Periods.Weekly)
        result shouldBe 120
      }

      "convert monthly amount to weekly" in {
        val result = helper.amountToWeeklyAmount(520, Periods.Monthly)
        result shouldBe 120
      }

      "convert yearly amount to weekly" in {
        val result = helper.amountToWeeklyAmount(6240, Periods.Yearly)
        result shouldBe 120
      }

      "return 0 if invalid period is given" in {
        val result = helper.amountToWeeklyAmount(6240, Periods.INVALID)
        result shouldBe 0
      }
    }

    "amountToAnnualAmount" should {
      "convert weekly amount to yearly" in {
        val result = helper.amountToAnnualAmount(120, Periods.Weekly)
        result shouldBe 6240
      }

      "convert monthly amount to yearly" in {
        val result = helper.amountToAnnualAmount(520, Periods.Monthly)
        result shouldBe 6240
      }

      "convert yearly amount to yearly" in {
        val result = helper.amountToAnnualAmount(6240, Periods.Yearly)
        result shouldBe 6240
      }

      "return 0 if invalid period is given" in {
        val result = helper.amountToAnnualAmount(6240, Periods.INVALID)
        result shouldBe 0
      }
    }

    "monthlyAmountToPeriod" should {
      "convert monthly amount to weekly" in {
        val result = helper.monthlyAmountToPeriod(520, Periods.Weekly)
        result shouldBe 120
      }

      "convert monthly amount to monthly" in {
        val result = helper.monthlyAmountToPeriod(520, Periods.Monthly)
        result shouldBe 520
      }

      "convert monthly amount to yearly" in {
        val result = helper.monthlyAmountToPeriod(520, Periods.Yearly)
        result shouldBe 6240
      }

      "return 0 if invalid period is given" in {
        val result = helper.monthlyAmountToPeriod(520, Periods.INVALID)
        result shouldBe 0
      }
    }

    "annualAmountToPeriod" should {
      "convert yearly amount to weekly" in {
        val result = helper.annualAmountToPeriod(6240, Periods.Weekly)
        result shouldBe 120
      }

      "convert yearly amount to monthly" in {
        val result = helper.annualAmountToPeriod(6240, Periods.Monthly)
        result shouldBe 520
      }

      "convert yearly amount to yearly" in {
        val result = helper.annualAmountToPeriod(6240, Periods.Yearly)
        result shouldBe 6240
      }

      "return 0 if invalid period is given" in {
        val result = helper.annualAmountToPeriod(6240, Periods.INVALID)
        result shouldBe 0
      }
    }
  }
}
