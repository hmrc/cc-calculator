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

import calculators.TestCalculator.TestCalculatorService
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, Periods}

import scala.concurrent.Future

/**
 * Created by adamconder on 08/06/15.
 */

trait TestCalculator extends CCCalculator {
  val calculator = new TestCalculatorService

  class TestCalculatorService extends CCCalculatorService {
    override def award(request: Request) : Future[AwardPeriod] = ???
  }
}

object TestCalculator extends TestCalculator

class CCCalculatorSpec extends UnitSpec with FakeCCCalculatorApplication {

  "CCCalculator" should {

    "return an instance of CCCalculatorService" in {
      val calc = TestCalculator
      calc.calculator.isInstanceOf[TestCalculatorService] shouldBe true
    }

    "calculate in a BigDecimal format" in {
      val cost = 12.249212342
      val result = TestCalculator.calculator.round(cost)
      result.isInstanceOf[BigDecimal] shouldBe true
    }

    "round up for double numbers if greater than 5" in {
      val cost: BigDecimal = 12.249212342
      val result: BigDecimal = TestCalculator.calculator.round(cost)
      result shouldBe 12.25
    }

    "(Pound precision) round up for double numbers if greater than 5" in {
      val cost : BigDecimal = 12.649212342
      val result : BigDecimal = TestCalculator.calculator.roundToPound(cost)
      result shouldBe 13
    }

    "round down for double numbers if less than 5" in {
      val cost: BigDecimal = 12.241212342
      val result: BigDecimal = TestCalculator.calculator.round(cost)
      result shouldBe 12.24
    }

    "round down for double numbers if less than 5 (.011)" in {
      val cost: BigDecimal = 12.01111111
      val result: BigDecimal = TestCalculator.calculator.round(cost)
      result shouldBe 12.01
    }

    "round down for double numbers if less than 5 (.444)" in {
      val cost: BigDecimal = 12.44444
      val result: BigDecimal = TestCalculator.calculator.round(cost)
      result shouldBe 12.44
    }

    "(Pound precision) round down for double numbers if less than 5 - 1" in {
      val cost : BigDecimal = 12.241212342
      val result : BigDecimal = TestCalculator.calculator.roundToPound(cost)
      result shouldBe 12
    }

    "(Pound precision) round down for double numbers if less than 5 - 2" in {
      val cost : BigDecimal = 12.445
      val result : BigDecimal = TestCalculator.calculator.roundToPound(cost)
      result shouldBe 12
    }

    "convert the amount to a quarterly amount (weekly)" in {
      val cost : BigDecimal = 200.00 // per week
      val result : BigDecimal = TestCalculator.calculator.amountToQuarterlyAmount(cost, Periods.Weekly)
      result shouldBe 2600.00
    }

    "convert the amount to a quarterly amount (fortnightly)" in {
      val cost : BigDecimal = 200.00 // per fortnight
      val result : BigDecimal = TestCalculator.calculator.amountToQuarterlyAmount(cost, Periods.Fortnightly)
      result shouldBe 1300.00
    }

    "convert the amount to a quarterly amount (monthly)" in {
      val cost : BigDecimal = 200.00 // per month
      val result : BigDecimal = TestCalculator.calculator.amountToQuarterlyAmount(cost, Periods.Monthly)
      result shouldBe 600.00
    }

    "convert the amount to a quarterly amount (quarterly)" in {
      val cost : BigDecimal  = 200.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountToQuarterlyAmount(cost, Periods.Quarterly)
      result shouldBe 200.00
    }

    "convert the amount to a quarterly amount (yearly)" in {
      val cost : BigDecimal = 200.00 // per year
      val result : BigDecimal = TestCalculator.calculator.amountToQuarterlyAmount(cost, Periods.Yearly)
      result shouldBe 50.00
    }

    "return the number of days between two dates" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-12-2014", formatter)
      val toDate = LocalDate.parse("12-12-2014", formatter)
      val result: Int = TestCalculator.calculator.daysBetween(fromDate, toDate)
      result shouldBe 11
    }

    "(leap year) return the number of days between two dates" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("20-02-2016", formatter)
      val toDate = LocalDate.parse("01-03-2016", formatter)
      val result: Int = TestCalculator.calculator.daysBetween(fromDate, toDate)
      result shouldBe 10
    }

  }
}
