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

package scenarios

import calculators.TCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.output.tc.{Element, Period}
import org.joda.time.LocalDate
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger}


trait ValidateCalculations {
  this: UnitSpec =>

  def verifyAmountsPerPeriod(period: Period, from: LocalDate, until: LocalDate, wtcWorkElement: Element, wtcChildcareElement: Element, ctcChildElement: Element, ctcFamilyElement: Element, periodNetAmount: BigDecimal = 0.00) = {
    // test the topup amount per period
    period.from shouldBe from
    period.until shouldBe until
    period.elements.wtcWorkElement shouldBe wtcWorkElement
    period.elements.wtcChildcareElement shouldBe wtcChildcareElement
    period.elements.ctcIndividualElement shouldBe ctcChildElement
    period.elements.ctcFamilyElement shouldBe ctcFamilyElement
    period.periodNetAmount shouldBe periodNetAmount
  }

  def verifyMaximumAmountsPerPeriod(period: Period, from: LocalDate, until: LocalDate, wtcWorkElementMax: BigDecimal = 0.00, wtcChildcareElementMax: BigDecimal = 0.00, ctcChildElementMax: BigDecimal = 0.00, ctcFamilyElementMax: BigDecimal = 0.00) = {
    // test the topup amount per period
    period.from shouldBe from
    period.until shouldBe until
    period.elements.wtcWorkElement.maximumAmount shouldBe wtcWorkElementMax
    period.elements.wtcChildcareElement.maximumAmount shouldBe wtcChildcareElementMax
    period.elements.ctcIndividualElement.maximumAmount shouldBe ctcChildElementMax
    period.elements.ctcFamilyElement.maximumAmount shouldBe ctcFamilyElementMax
  }

}

class TCScenarioSpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger with ValidateCalculations {

  "ScenarioSpec" should {

  "(TY 2016/2017 Scenario 1) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2078.08, taperAmount = 2078.08)
              val wtcChildcareElement = Element(netAmount = 686.28, maximumAmount = 878.41, taperAmount = 192.13)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 2426.29
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 2426.29
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14819.1500
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 2) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_2.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)

      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2078.08, taperAmount = 2078.08)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 3333.14, taperAmount = 3333.14)
              val ctcChildElement = Element(netAmount = 949.54, maximumAmount = 1455.42, taperAmount = 505.88)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 1234.13
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 1234.13
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 20808.6912
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 3) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 4) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_4.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 5) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_5.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 6) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_6.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 7) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_7.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 231.89, maximumAmount = 2502.10, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 2850.31
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 2850.31
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 15853.7588
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00

    }

    "(TY 2016/2017 Scenario 8) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_8.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2502.10, taperAmount = 2502.10)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 878.41, taperAmount = 878.41)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 1455.42, taperAmount = 1455.42)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 284.59, taperAmount = 284.59)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 0.00
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 15853.7588
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 9) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_9.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 10) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_10.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true
      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 11) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_11.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 12) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_12.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true
      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 13) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_13.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 1362.61, maximumAmount = 3632.82, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 3981.03
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 3981.03
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 18612.7156
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 14) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_14.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 5187.56, taperAmount = 5187.56)
              val wtcChildcareElement = Element(netAmount = 148.87, maximumAmount = 878.41, taperAmount = 729.54)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 1888.88
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 1888.88
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 22406.2812
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 15) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_15.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true
      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 3632.82,  878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 16) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_16.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5187.56, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 17) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_17.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 3632.82, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 18) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5187.56, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 19) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_19.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 2029.20, maximumAmount = 4299.41, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 4647.62
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 4647.62
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 20239.1952
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 20) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_20.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 603.64, maximumAmount = 6520.74, taperAmount = 5917.10)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 3222.06
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 3222.06
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 25659.2404
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 21) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_21.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4299.41, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 22) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_22.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6520.74, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 23) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_23.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4299.41, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }


    "(TY 2016/2017 Scenario 24) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_24.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6520.74, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 25) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_25.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 878.41, 1455.42, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 26) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_26.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 878.41, 1455.42, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 27) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_27.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 28) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_28.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 29) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_29.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84,  878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 30) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_30.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 878.41, 3764.61, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 31) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_31.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4723.43, 878.41, 1455.42, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 32) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_32.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6944.76, 878.41, 1455.42, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 33) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_33.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4723.43, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 34) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_34.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6944.76, 878.41, 3098.02, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 35) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_35.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 2453.22, maximumAmount = 4723.43, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 3764.61, maximumAmount = 3764.61, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 7380.83
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 7380.83
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 26908.2276
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 36) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_36.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 1027.66, maximumAmount = 6944.76, taperAmount = 5917.10)
              val wtcChildcareElement = Element(netAmount = 878.41, maximumAmount = 878.41, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 3764.61, maximumAmount = 3764.61, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 5955.27
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 5955.27
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 32328.2728
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 37) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_37.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5714.34, 2910.84, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }


    "(TY 2016/2017 Scenario 38) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_38.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5700.97, 4553.44, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 39) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_39.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5687.60, 5220.03, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 40) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_40.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1758.16, 6196.04, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 41) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_41.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1758.16, 6862.63, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 42) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_42.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1758.16, 7529.22, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 43) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_43.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 2637.90, 6008.86, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 44) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_44.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 2029.20, maximumAmount = 4299.41, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 5714.34 , maximumAmount = 5714.34 , taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 8318.05, maximumAmount = 8318.05, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00)
              val periodNetAmount = 16346.18
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 16346.18
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 48783.6816
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 45) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_45.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4299.41, 2637.90, 6675.45, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 46) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_46.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 2637.90, 9960.65, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 47) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_47.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6944.76, 1758.16, 5220.03, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }


    "(TY 2016/2017 Scenario 48) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_48.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4299.41, 1758.16, 6862.63, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 49) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_49.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 1758.16, 7529.22, 284.59)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 50) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_50.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate,995.60, 699.58,  1158.24,  113.24)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 1506.50, 528.88, 876.30, 171.35)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
    }

    "(TY 2016/2017 Scenario 51) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
//      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 995.60, netAmount = 92.27, taperAmount = 903.33)
              val wtcChildcareElement = Element(maximumAmount = 699.58, netAmount = 699.58, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 2077.08, netAmount = 2077.08, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 113.24, netAmount = 113.24, taperAmount = 0.00)
              val periodNetAmount = 2982.17
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 872.85, netAmount = 0.00, taperAmount = 872.85)
              val wtcChildcareElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 2982.17
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14970.0640
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 52) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 995.60, netAmount = 92.27, taperAmount = 903.33)
              val wtcChildcareElement = Element(maximumAmount = 349.52, netAmount = 349.52, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 1232.72, netAmount = 1232.72, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 113.24, netAmount = 113.24, taperAmount = 0.00)
              val periodNetAmount = 1787.75
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 872.85, netAmount = 0.00, taperAmount = 872.85)
              val wtcChildcareElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 1787.75
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 12055.6792
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 53) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_53.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 1614.24, netAmount = 0.00, taperAmount = 1614.24)
              val wtcChildcareElement = Element(maximumAmount = 349.52, netAmount = 0.00, taperAmount = 349.52)
              val ctcChildElement = Element(maximumAmount = 1232.72, netAmount = 842.03, taperAmount = 390.69)
              val ctcFamilyElement = Element(maximumAmount = 113.24, netAmount = 113.24, taperAmount = 0.00)
              val periodNetAmount = 955.27
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 2442.60, netAmount = 0.00, taperAmount = 2442.60)
              val wtcChildcareElement = Element(maximumAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 0.00)

              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement)
            case _ => throw new Exception // fail test
          }
        }
      }catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 955.27
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 17395.3508
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 54) Generate total award with periods with elements (not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_54.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 1025.67, taperAmount = 1025.67)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 0.00
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5862.3248
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 55) Generate total award with periods with elements (not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_55.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2580.41, taperAmount = 2580.41)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)

            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 0.00
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 9655.8904
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 56) Generate total award with periods with elements (not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_56.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 1025.67, maximumAmount = 1025.67, taperAmount = 0.00)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 1025.67
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)

            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 1025.67
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5862.3248
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 57) Generate total award with periods with elements (not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_57.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 1025.67, maximumAmount = 1025.67, taperAmount = 0.00)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 1025.67
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)

            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 1025.67
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5862.3248
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 58, Child being born mid tax year) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_58.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 117.90, taperAmount = 117.90)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3589.40, taperAmount = 3589.40)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1260.13, taperAmount = 1260.13)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 2087.88, taperAmount = 2087.88)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 408.26, taperAmount = 408.26)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 0.00
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23189.0808
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 59, doesNotTaper is true) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_59.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 117.90, maximumAmount = 117.90, taperAmount = 0.00)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 117.90
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 3589.40, maximumAmount = 3589.40, taperAmount = 0.00)
              val wtcChildcareElement = Element(netAmount = 1260.13, maximumAmount = 1260.13, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 2087.88, maximumAmount = 2087.88, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 408.26, maximumAmount = 408.26, taperAmount = 0.00)
              val periodNetAmount = 7345.67
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 7463.57
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23189.0808
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 60, doesNotTaper is false) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_60.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 117.90, taperAmount = 117.90)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3589.40, taperAmount = 3589.40)
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1260.13, taperAmount = 1260.13)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 2087.88, taperAmount = 2087.88)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 408.26, taperAmount = 408.26)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 0.00
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23189.0808
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 Scenario 61) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_61.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val taxYearProRataAward = TCCalculator.calculator.award(result.get)
      val taxYearProRataIncomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = taxYearProRataAward.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 995.60, netAmount = 92.27, taperAmount = 903.33)
              val wtcChildcareElement = Element(maximumAmount = 699.58, netAmount = 699.58, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 2077.08, netAmount = 2077.08, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 113.24, netAmount = 113.24, taperAmount = 0.00)
              val periodNetAmount = 2982.17
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              val wtcWorkElement = Element(maximumAmount = 872.85, netAmount = 0.00, taperAmount = 872.85)
              val wtcChildcareElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 0.00, netAmount = 0.00, taperAmount = 0.00)
              val periodNetAmount = 0.00
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      taxYearProRataAward.tc.get.totalAwardAmount shouldBe 2982.17
      taxYearProRataAward.tc.get.totalAwardProRataAmount shouldBe 624.54
      taxYearProRataAward.tc.get.houseHoldAdviceAmount shouldBe 0.00
      taxYearProRataIncomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14970.0640
      taxYearProRataIncomeAdvice.tc.get.totalHouseHoldAdviceProRataAmount shouldBe 3135.09
      taxYearProRataIncomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2017/2018 Scenario 62) Generate total award with periods with elements" in {
        val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_1.json")
        val json: JsValue = Json.parse(resource.toString)
        val result = json.validate[Request]
        logResult(result)
        result.isInstanceOf[JsSuccess[Request]] shouldBe true

        val award = TCCalculator.calculator.award(result.get)
        val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

         try {
          val tys = award.tc.get.taxYears.zipWithIndex

            for ((ty, i) <- tys) {
              i match {
                case 0 =>
                  ty.taxYearAdviceAmount shouldBe BigDecimal(0.00)
                  ty.taxYearAwardAmount shouldBe BigDecimal(2426.29)

                 // loop over each period checking it's values
                val periods: List[Period] = ty.periods
                for ((period, i) <- periods.zipWithIndex) {
                i match {
                  case 0 =>
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2078.08, taperAmount = 2078.08)
                    val wtcChildcareElement = Element(netAmount = 686.28, maximumAmount = 878.41, taperAmount = 192.13)
                    val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 284.59, maximumAmount = 284.59, taperAmount = 0.00) // no taper rate?
                    val periodNetAmount = 2426.29
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                 case _ => throw new Exception // fail test
                 }
                }
               case _ => throw new Exception
             }
           }
         } catch {
           case e: Exception =>
              throw e
        }

       award.tc.get.totalAwardAmount shouldBe 2426.29
       award.tc.get.houseHoldAdviceAmount shouldBe 0.00

       incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14819.1500
       incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2017/2018 Scenario 63) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_2.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        // loop over each period checking it's values
        val periods: List[Period] = award.tc.get.taxYears.head.periods
        for ((period, i) <- periods.zipWithIndex) {
          i match {
            case 0 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until

              val wtcWorkElement = Element(netAmount = 92.27, maximumAmount = 995.6, taperAmount = 903.33)
              val wtcChildcareElement = Element(netAmount = 699.58, maximumAmount = 699.58, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1158.24, maximumAmount = 1158.24, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 113.24, maximumAmount = 113.24, taperAmount = 0.00)
              val periodNetAmount = 2063.33
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until

              val wtcWorkElement = Element(netAmount = 139.62, maximumAmount = 1506.50, taperAmount = 1366.88)
              val wtcChildcareElement = Element(netAmount = 528.88, maximumAmount = 528.88, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 876.30, maximumAmount = 876.30, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 171.35, maximumAmount = 171.35, taperAmount = 0.00)
              val periodNetAmount = 1716.15
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 3779.48
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 18120.9336
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 - 2017/2018 Scenario 64) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        val taxYears = award.tc.get.taxYears

        for ((ty, ti) <- taxYears.zipWithIndex) {
          ti match {
            case 0 =>
              // first tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                 pi match {
                   case 0 =>
                     // first period
                     val fromDate : LocalDate = period.from
                     val toDate : LocalDate = period.until
                     val wtcWorkElement = Element(netAmount = 92.27, maximumAmount = 995.60, taperAmount = 903.33)
                     val wtcChildcareElement = Element(netAmount = 1224.13, maximumAmount = 1224.13, taperAmount = 0.00)
                     val ctcChildElement = Element(netAmount = 1737.36, maximumAmount = 1737.36, taperAmount = 0.00)
                     val ctcFamilyElement = Element(netAmount = 113.24, maximumAmount = 113.24, taperAmount = 0.00)
                     val periodNetAmount = 3167
                     verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                   case 1 =>
                     // second period
                     val fromDate : LocalDate = period.from
                     val toDate : LocalDate = period.until
                     val wtcWorkElement = Element(netAmount = 139.62, maximumAmount = 1506.50, taperAmount = 1366.88)
                     val wtcChildcareElement = Element(netAmount = 1058.58, maximumAmount = 1058.58, taperAmount = 0.00)
                     val ctcChildElement = Element(netAmount = 1752.60, maximumAmount = 1752.60, taperAmount = 0.00)
                     val ctcFamilyElement = Element(netAmount = 171.35, maximumAmount = 171.35, taperAmount = 0.00)
                     val periodNetAmount = 3122.15
                     verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                 }
              }
            case 1 =>
              // second tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                pi match {
                  case 0 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 179.69, maximumAmount = 1938.80, taperAmount = 1759.11)
                    val wtcChildcareElement = Element(netAmount = 1362.34, maximumAmount = 1362.34, taperAmount = 0.00)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 220.52, maximumAmount = 220.52, taperAmount = 0.00)
                    val periodNetAmount = 4018.07
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                  // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
                    val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 0.00, taperAmount = 0.00)
                    val periodNetAmount = 0.00
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
               }
            }
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 10307.22
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 40944.1676
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 - 2017/2018 Scenario 65) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_4.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)
      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {
        val taxYears = award.tc.get.taxYears

        for ((ty, ti) <- taxYears.zipWithIndex) {
          ti match {
            case 0 =>
              // first tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                pi match {
                  case 0 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 1879.48, taperAmount = 1879.48)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1224.13, taperAmount = 1224.13)
                    val ctcChildElement = Element(netAmount = 1120.47, maximumAmount = 1737.36, taperAmount = 616.89)
                    val ctcFamilyElement = Element(netAmount = 113.24, maximumAmount = 113.24, taperAmount = 0.00)
                    val periodNetAmount = 1233.71
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2843.95, taperAmount = 2843.95)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1058.58, taperAmount = 1058.58)
                    val ctcChildElement = Element(netAmount = 25.42, maximumAmount = 1752.60, taperAmount = 1727.18)
                    val ctcFamilyElement = Element(netAmount = 171.35, maximumAmount = 171.35, taperAmount = 0.00)
                    val periodNetAmount = 196.77
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
            case 1 =>
              // second tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                pi match {
                  case 0 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3660.04, taperAmount = 3660.04)
                    val wtcChildcareElement = Element(netAmount = 271.14, maximumAmount = 1362.34, taperAmount = 1091.20)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 220.52, maximumAmount = 220.52, taperAmount = 0.00)
                    val periodNetAmount = 2747.18
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 4129.91, taperAmount = 4129.91)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 768.03, taperAmount = 768.03)
                    val ctcChildElement = Element(netAmount = 809.28, maximumAmount = 1272.54, taperAmount = 463.26)
                    val ctcFamilyElement = Element(netAmount = 248.83, maximumAmount = 248.83, taperAmount = 0.00)
                    val periodNetAmount = 1058.11
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 5235.77
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 69164.6848
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(Proratering) (TY 2016/2017 - 2017/2018 Scenario 66) Generate total award with periods with elements" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_5.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result.isInstanceOf[JsSuccess[Request]] shouldBe true

      val award = TCCalculator.calculator.award(result.get)

      val incomeAdvice = TCCalculator.calculator.incomeAdvice(result.get)

      try {

        val taxYears = award.tc.get.taxYears

        for ((ty, ti) <- taxYears.zipWithIndex) {
          ti match {
            case 0 =>
              // first tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                pi match {
                  case 0 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 1879.48, taperAmount = 1879.48)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1224.13, taperAmount = 1224.13)
                    val ctcChildElement = Element(netAmount = 1120.47, maximumAmount = 1737.36, taperAmount = 616.89)
                    val ctcFamilyElement = Element(netAmount = 113.24, maximumAmount = 113.24, taperAmount = 0.00)
                    val periodNetAmount = 1233.71
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2843.95, taperAmount = 2843.95)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1058.58, taperAmount = 1058.58)
                    val ctcChildElement = Element(netAmount = 25.42, maximumAmount = 1752.60, taperAmount = 1727.18)
                    val ctcFamilyElement = Element(netAmount = 171.35, maximumAmount = 171.35, taperAmount = 0.00)
                    val periodNetAmount = 196.77
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
            case 1 =>
              // second tax year
              val periods = ty.periods.zipWithIndex
              for ((period, pi) <- periods) {
                pi match {
                  case 0 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3660.04, taperAmount = 3660.04)
                    val wtcChildcareElement = Element(netAmount = 271.14, maximumAmount = 1362.34, taperAmount = 1091.20)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 220.52, maximumAmount = 220.52, taperAmount = 0.00)
                    val periodNetAmount = 2747.18
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 4129.91, taperAmount = 4129.91)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 768.03, taperAmount = 768.03)
                    val ctcChildElement = Element(netAmount = 809.28, maximumAmount = 1272.54, taperAmount = 463.26)
                    val ctcFamilyElement = Element(netAmount = 248.83, maximumAmount = 248.83, taperAmount = 0.00)
                    val periodNetAmount = 1058.11
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 5235.77
      award.tc.get.taxYears.tail.head.taxYearAwardProRataAmount shouldBe 1389.23
      award.tc.get.totalAwardProRataAmount shouldBe 2819.71
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 69164.6848
      incomeAdvice.tc.get.taxYears.head.taxYearAdviceAmount shouldBe 29664.5736
      incomeAdvice.tc.get.taxYears.tail.head.taxYearAdviceAmount shouldBe 39500.1112 //11820.95
      incomeAdvice.tc.get.totalHouseHoldAdviceProRataAmount shouldBe 44085.2536
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

  }
}
