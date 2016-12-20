/*
 * Copyright 2016 HM Revenue & Customs
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

    "reduce by 1 pence when differences is within 1 pence" in {
      val amount = 33.34
      val originalSpend = 33.33
      val (result, _): (BigDecimal, Boolean) = TestCalculator.calculator.verifyPenceDifference(amount, originalSpend)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£33.33"
    }

    "not reduce by 1 pence when the difference is greater" in {
      val amount = 33.34
      val originalSpend = 43.33
      val (result, _): (BigDecimal, Boolean) = TestCalculator.calculator.verifyPenceDifference(amount, originalSpend)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£33.34"
    }

    "not reduce by 1 pence when the difference is smaller" in {
      val amount = 33.34
      val originalSpend = 43.33
      val (result, _): (BigDecimal, Boolean) = TestCalculator.calculator.verifyPenceDifference(amount, originalSpend)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£33.34"
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

    "convert the quarterly amount to a period (weekly)" in {
      val cost : BigDecimal = 2000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.quarterlyAmountToPeriod(cost, Periods.Weekly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£153.85"
    }

    "convert the quarterly amount to a period (fortnightly)" in {
      val cost : BigDecimal = 2000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.quarterlyAmountToPeriod(cost, Periods.Fortnightly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£307.69"
    }

    "convert the quarterly amount to a period (monthly)" in {
      val cost : BigDecimal = 2000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.quarterlyAmountToPeriod(cost, Periods.Monthly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£666.67"
    }

    "convert the quarterly amount to a period (quarterly)" in {
      val cost : BigDecimal = 2000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.quarterlyAmountToPeriod(cost, Periods.Quarterly)
      result shouldBe 2000.00
    }

    "convert the quarterly amount to a period (yearly)" in {
      val cost : BigDecimal = 2000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.quarterlyAmountToPeriod(cost, Periods.Yearly)
      result shouldBe 8000.00
    }

    "convert the daily amount to a period (weekly)" in {
      val cost : BigDecimal = 1000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountFromPeriodToDaily(cost, Periods.Weekly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£142.47"
    }

    "convert the daily amount to a period (fortnightly)" in {
      val cost : BigDecimal = 1000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountFromPeriodToDaily(cost, Periods.Fortnightly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£71.23"
    }

    "convert the daily amount to a period (monthly)" in {
      val cost : BigDecimal = 1000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountFromPeriodToDaily(cost, Periods.Monthly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£32.88"
    }

    "convert the daily amount to a period (quarterly)" in {
      val cost : BigDecimal = 1000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountFromPeriodToDaily(cost, Periods.Quarterly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£10.96"
    }

    "convert the daily amount to a period (yearly)" in {
      val cost : BigDecimal = 1000.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountFromPeriodToDaily(cost, Periods.Yearly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£2.74"
    }

    "convert weekly amount to a Monthly amount" in {
      val cost : BigDecimal = 100.00 // per week
      val result : BigDecimal = TestCalculator.calculator.amountToMonthlyAmount(cost, Periods.Weekly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£433.33"
    }

    "convert monthly amount to a Monthly amount" in {
      val cost : BigDecimal = 100.00 // per month
      val result : BigDecimal = TestCalculator.calculator.amountToMonthlyAmount(cost, Periods.Monthly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£100.00"
    }

    "convert fortnightly amount to a Monthly amount" in {
      val cost : BigDecimal = 100.00 // per fortnight
      val result : BigDecimal = TestCalculator.calculator.amountToMonthlyAmount(cost, Periods.Fortnightly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£216.67"
    }

    "convert quarterly amount to a Monthly amount" in {
      val cost : BigDecimal = 500.00 // per quarter
      val result : BigDecimal = TestCalculator.calculator.amountToMonthlyAmount(cost, Periods.Quarterly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£125.00"
    }

    "convert yearly amount to a Monthly amount" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.amountToMonthlyAmount(cost, Periods.Yearly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£83.33"
    }

    "convert annual amount to a Period (week)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.annualAmountToPeriod(cost, Periods.Weekly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£19.23"
    }

    "convert annual amount to a Period (fortnight)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.annualAmountToPeriod(cost, Periods.Fortnightly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£38.46"
    }

    "convert annual amount to a Period (month)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.annualAmountToPeriod(cost, Periods.Monthly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£83.33"
    }

    "convert annual amount to a Period (quarter)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.annualAmountToPeriod(cost, Periods.Quarterly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£250.00"
    }

    "convert annual amount to a Period (yearly)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.annualAmountToPeriod(cost, Periods.Yearly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£1,000.00"
    }

    "convert monthly amount to a Period (week)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.monthlyAmountToPeriod(cost, Periods.Weekly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£230.77"
    }

    "convert monthly amount to a Period (fortnight)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.monthlyAmountToPeriod(cost, Periods.Fortnightly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£461.54"
    }

    "convert monthly amount to a Period (month)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.monthlyAmountToPeriod(cost, Periods.Monthly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£1,000.00"
    }

    "convert monthly amount to a Period (quarter)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.monthlyAmountToPeriod(cost, Periods.Quarterly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£4,000.00"
    }

    "convert monthly amount to a Period (year)" in {
      val cost : BigDecimal = 1000.00 // per annum
      val result : BigDecimal = TestCalculator.calculator.monthlyAmountToPeriod(cost, Periods.Yearly)
      TestCalculator.calculator.convertToCurrency(result) shouldBe "£12,000.00"
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
