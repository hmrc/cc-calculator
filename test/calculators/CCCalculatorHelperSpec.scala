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

    "convert the amount to a quarterly amount (fortnightly)" in {
      val cost : BigDecimal = 200.00 // per fortnight
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Fortnightly)
      result shouldBe 1300.00
    }

    "convert the amount to a quarterly amount (monthly)" in {
      val cost : BigDecimal = 200.00 // per month
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Monthly)
      result shouldBe 600.00
    }

    "convert the amount to a quarterly amount (quarterly)" in {
      val cost : BigDecimal  = 200.00 // per quarter
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Quarterly)
      result shouldBe 200.00
    }

    "convert the amount to a quarterly amount (yearly)" in {
      val cost : BigDecimal = 200.00 // per year
      val result : BigDecimal = helper.amountToQuarterlyAmount(cost, Periods.Yearly)
      result shouldBe 50.00
    }

    "return the number of days between two dates" in {
      val fromDate = LocalDate.now()
      val toDate = fromDate.plusDays(11)
      val result: Int = helper.daysBetween(fromDate, toDate)
      result shouldBe 11
    }

  }
}
