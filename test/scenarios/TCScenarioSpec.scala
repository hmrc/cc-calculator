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

package test

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
              val wtcChildcareElement = Element(netAmount = 703.11, maximumAmount = 895.24, taperAmount = 192.13)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 2445.03
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 2445.03
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14860.17
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
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 3333.35, taperAmount = 3333.35)
              val ctcChildElement = Element(netAmount = 949.74, maximumAmount = 1455.42, taperAmount = 505.68)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 1236.24
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 1236.24
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 20806.72
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2078.08, 895.24, 3768.43, 286.50)
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
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 2869.05
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 2869.05
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 15894.35
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
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 895.24, taperAmount = 895.24)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 1455.42, taperAmount = 1455.42)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 286.50, taperAmount = 286.50)
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
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 15894.35
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 895.24, 3768.43, 286.50)
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
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 3999.77
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 3999.77
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 18652.18
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
              val wtcChildcareElement = Element(netAmount = 165.69, maximumAmount = 895.24, taperAmount = 729.55)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 1907.61
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 1907.61
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 22444.19
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 3632.82,  895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5187.56, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 3632.82, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5187.56, 895.24, 3768.43, 286.50)
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
              val wtcWorkElement = Element(netAmount = 2031.11, maximumAmount = 4301.32, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 4668.27
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 4668.27
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 20282.65
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
              val wtcWorkElement = Element(netAmount = 607.45, maximumAmount = 6524.56, taperAmount = 5917.11)
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 3244.61
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 3244.61
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 25705.13
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4301.32, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6524.56, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4301.32, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6524.56, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 895.24, 1455.42, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 895.24, 1455.42, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84,  895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 5611.58, 895.24, 3768.43, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4725.34, 895.24, 1455.42, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6948.58, 895.24, 1455.42, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4725.34, 895.24, 3099.93, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6948.58, 895.24, 3099.93, 286.50)
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
              val wtcWorkElement = Element(netAmount = 2455.13, maximumAmount = 4725.34, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 3768.43, maximumAmount = 3768.43, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 7405.30
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 7405.30
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 26958.26
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
              val wtcWorkElement = Element(netAmount = 1031.47, maximumAmount = 6948.58, taperAmount = 5917.11)
              val wtcChildcareElement = Element(netAmount = 895.24, maximumAmount = 895.24, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 3768.43, maximumAmount = 3768.43, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 5981.64
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 5981.64
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 32380.75
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5714.31 , 2910.84, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5714.31 , 4555.35, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 5695.26, 5223.85, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1771.44, 6199.86, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1771.44, 6868.36, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 1771.44, 7536.86, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 2647.63, 6010.77, 286.50)
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
              val wtcWorkElement = Element(netAmount = 2031.11, maximumAmount = 4301.32, taperAmount = 2270.21)
              val wtcChildcareElement = Element(netAmount = 5714.31 , maximumAmount = 5714.31 , taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 8323.78, maximumAmount = 8323.78, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00)
              val periodNetAmount = 16355.70
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 16355.70
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 48788.29
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4301.32, 2647.63, 6679.27, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 2502.10, 2647.63, 9968.29, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 6948.58, 1771.44, 5223.85, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4301.32, 1771.44, 6868.36, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 4056.84, 1771.44, 7536.86, 286.50)
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
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate,995.60, 704.87,  1158.24,  114.00)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until
              verifyMaximumAmountsPerPeriod(period, fromDate, toDate, 1506.50, 539.02, 876.30, 172.50)
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
              val wtcChildcareElement = Element(maximumAmount = 704.87, netAmount = 704.87, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 2078.60, netAmount = 2078.60, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 114.00, netAmount = 114.00, taperAmount = 0.00)
              val periodNetAmount = 2989.74
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
      award.tc.get.totalAwardAmount shouldBe 2989.74
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14983.78
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
              val wtcChildcareElement = Element(maximumAmount = 356.23, netAmount = 356.23, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 1233.48, netAmount = 1233.48, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 114.00, netAmount = 114.00, taperAmount = 0.00)
              val periodNetAmount = 1795.98
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
      award.tc.get.totalAwardAmount shouldBe 1795.98
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 12072.20
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
              val wtcChildcareElement = Element(maximumAmount = 356.23, netAmount = 0.00, taperAmount = 356.23)
              val ctcChildElement = Element(maximumAmount = 1233.48, netAmount = 849.50, taperAmount = 383.98)
              val ctcFamilyElement = Element(maximumAmount = 114.00, netAmount = 114.00, taperAmount = 0.00)
              val periodNetAmount = 963.50
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
      award.tc.get.totalAwardAmount shouldBe 963.50
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 17409.69
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
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5861.30
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
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 9653.31
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
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5861.30
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
      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 5861.30
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
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1284.28, taperAmount = 1284.28)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 2087.88, taperAmount = 2087.88)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 411.00, taperAmount = 411.00)
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

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23247.21
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
              val wtcChildcareElement = Element(netAmount = 1284.28, maximumAmount = 1284.28, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 2087.88, maximumAmount = 2087.88, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 411.00, maximumAmount = 411.00, taperAmount = 0.00)
              val periodNetAmount = 7372.56
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }
      award.tc.get.totalAwardAmount shouldBe 7490.46
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23247.21
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
              val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1284.28, taperAmount = 1284.28)
              val ctcChildElement = Element(netAmount = 0.00, maximumAmount = 2087.88, taperAmount = 2087.88)
              val ctcFamilyElement = Element(netAmount = 0.00, maximumAmount = 411.00, taperAmount = 411.00)
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

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 23247.21
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
              val wtcChildcareElement = Element(maximumAmount = 704.87, netAmount = 704.87, taperAmount = 0.00)
              val ctcChildElement = Element(maximumAmount = 2078.60, netAmount = 2078.60, taperAmount = 0.00)
              val ctcFamilyElement = Element(maximumAmount = 114.00, netAmount = 114.00, taperAmount = 0.00)
              val periodNetAmount = 2989.74
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
      taxYearProRataAward.tc.get.totalAwardAmount shouldBe 2989.74
      taxYearProRataAward.tc.get.totalAwardProRataAmount shouldBe 626.13
      taxYearProRataAward.tc.get.houseHoldAdviceAmount shouldBe 0.00
      taxYearProRataIncomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14983.78
      taxYearProRataIncomeAdvice.tc.get.totalHouseHoldAdviceProRataAmount shouldBe 3137.97
      taxYearProRataIncomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2017/2018 Scenario 1) Generate total award with periods with elements" in {
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
                  ty.taxYearAwardAmount shouldBe BigDecimal(2445.03)

                 // loop over each period checking it's values
                val periods: List[Period] = ty.periods
                for ((period, i) <- periods.zipWithIndex) {
                i match {
                  case 0 =>
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until
                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2078.08, taperAmount = 2078.08)
                    val wtcChildcareElement = Element(netAmount = 703.11, maximumAmount = 895.24, taperAmount = 192.13)
                    val ctcChildElement = Element(netAmount = 1455.42, maximumAmount = 1455.42, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 286.50, maximumAmount = 286.50, taperAmount = 0.00) // no taper rate?
                    val periodNetAmount = 2445.03
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

       award.tc.get.totalAwardAmount shouldBe 2445.03
       award.tc.get.houseHoldAdviceAmount shouldBe 0.00

       incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 14860.17
       incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2017/2018 Scenario 2) Generate total award with periods with elements" in {
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
              val wtcChildcareElement = Element(netAmount = 704.87, maximumAmount = 704.87, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 1158.24, maximumAmount = 1158.24, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 114.00, maximumAmount = 114.00, taperAmount = 0.00)
              val periodNetAmount = 2069.38
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case 1 =>
              val fromDate : LocalDate = period.from
              val toDate : LocalDate = period.until

              val wtcWorkElement = Element(netAmount = 139.62, maximumAmount = 1506.50, taperAmount = 1366.88)
              val wtcChildcareElement = Element(netAmount = 539.02, maximumAmount = 539.02, taperAmount = 0.00)
              val ctcChildElement = Element(netAmount = 876.30, maximumAmount = 876.30, taperAmount = 0.00)
              val ctcFamilyElement = Element(netAmount = 172.50, maximumAmount = 172.50, taperAmount = 0.00)
              val periodNetAmount = 1727.44
              verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
            case _ => throw new Exception // fail test
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 3796.82
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 18157.18
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 - 2017/2018 Scenario 3) Generate total award with periods with elements" in {
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
                     val wtcChildcareElement = Element(netAmount = 1227.83, maximumAmount = 1227.83, taperAmount = 0.00)
                     val ctcChildElement = Element(netAmount = 1737.36, maximumAmount = 1737.36, taperAmount = 0.00)
                     val ctcFamilyElement = Element(netAmount = 114.00, maximumAmount = 114.00, taperAmount = 0.00)
                     val periodNetAmount = 3171.46
                     verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                   case 1 =>
                     // second period
                     val fromDate : LocalDate = period.from
                     val toDate : LocalDate = period.until

                     val wtcWorkElement = Element(netAmount = 139.62, maximumAmount = 1506.50, taperAmount = 1366.88)
                     val wtcChildcareElement = Element(netAmount = 1066.57, maximumAmount = 1066.57, taperAmount = 0.00)
                     val ctcChildElement = Element(netAmount = 1752.60, maximumAmount = 1752.60, taperAmount = 0.00)
                     val ctcFamilyElement = Element(netAmount = 172.50, maximumAmount = 172.50, taperAmount = 0.00)
                     val periodNetAmount = 3131.29
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

                    val wtcWorkElement = Element(netAmount = 179.68, maximumAmount = 1938.80, taperAmount = 1759.12)
                    val wtcChildcareElement = Element(netAmount = 1372.63, maximumAmount = 1372.63, taperAmount = 0.00)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 222, maximumAmount = 222, taperAmount = 0.00)
                    val periodNetAmount = 4029.83
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

      award.tc.get.totalAwardAmount shouldBe 10332.58
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 40991.72
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(TY 2016/2017 - 2017/2018 Scenario 4) Generate total award with periods with elements" in {
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

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 1880.24, taperAmount = 1880.24)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1227.83, taperAmount = 1227.83)
                    val ctcChildElement = Element(netAmount = 1124.92, maximumAmount = 1737.36, taperAmount = 612.44)
                    val ctcFamilyElement = Element(netAmount = 114.00, maximumAmount = 114.00, taperAmount = 0.00)
                    val periodNetAmount = 1238.92
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2845.10, taperAmount = 2845.10)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1066.57, taperAmount = 1066.57)
                    val ctcChildElement = Element(netAmount = 34.56, maximumAmount = 1752.60, taperAmount = 1718.04)
                    val ctcFamilyElement = Element(netAmount = 172.50, maximumAmount = 172.50, taperAmount = 0.00)
                    val periodNetAmount = 207.06
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

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3661.52, taperAmount = 3661.52)
                    val wtcChildcareElement = Element(netAmount = 282.90, maximumAmount = 1372.63, taperAmount = 1089.73)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount =  0.00)
                    val ctcFamilyElement = Element(netAmount = 222, maximumAmount = 222, taperAmount = 0.00)
                    val periodNetAmount = 2760.42
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // first period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 4131.58, taperAmount = 4131.58)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 782.75, taperAmount = 782.75)
                    val ctcChildElement = Element(netAmount = 825.66, maximumAmount = 1272.54, taperAmount = 446.88)
                    val ctcFamilyElement = Element(netAmount = 250.50, maximumAmount = 250.5, taperAmount = 0.00)
                    val periodNetAmount = 1076.16
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 5282.56
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 69254.20
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

    "(Proratering) (TY 2016/2017 - 2017/2018 Scenario 5) Generate total award with periods with elements" in {
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

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 1880.24, taperAmount = 1880.24)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1227.83, taperAmount = 1227.83)
                    val ctcChildElement = Element(netAmount = 1124.92, maximumAmount = 1737.36, taperAmount = 612.44)
                    val ctcFamilyElement = Element(netAmount = 114.00, maximumAmount = 114.00, taperAmount = 0.00)
                    val periodNetAmount = 1238.92
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 2845.10, taperAmount = 2845.10)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 1066.57, taperAmount = 1066.57)
                    val ctcChildElement = Element(netAmount = 34.56, maximumAmount = 1752.60, taperAmount = 1718.04)
                    val ctcFamilyElement = Element(netAmount = 172.50, maximumAmount = 172.50, taperAmount = 0.00)
                    val periodNetAmount = 207.06
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

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 3661.52, taperAmount = 3661.52)
                    val wtcChildcareElement = Element(netAmount = 282.90, maximumAmount = 1372.63, taperAmount = 1089.73)
                    val ctcChildElement = Element(netAmount = 2255.52, maximumAmount = 2255.52, taperAmount = 0.00)
                    val ctcFamilyElement = Element(netAmount = 222, maximumAmount = 222, taperAmount = 0.00)
                    val periodNetAmount = 2760.42
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                  case 1 =>
                    // second period
                    val fromDate : LocalDate = period.from
                    val toDate : LocalDate = period.until

                    val wtcWorkElement = Element(netAmount = 0.00, maximumAmount = 4131.58, taperAmount = 4131.58)
                    val wtcChildcareElement = Element(netAmount = 0.00, maximumAmount = 782.75, taperAmount = 782.75)
                    val ctcChildElement = Element(netAmount = 825.66, maximumAmount = 1272.54, taperAmount = 446.88)
                    val ctcFamilyElement = Element(netAmount = 250.50, maximumAmount = 250.5, taperAmount = 0.00)
                    val periodNetAmount = 1076.16
                    verifyAmountsPerPeriod(period, fromDate, toDate, wtcWorkElement, wtcChildcareElement, ctcChildElement, ctcFamilyElement, periodNetAmount)
                }
              }
          }
        }
      } catch {
        case e: Exception =>
          throw e
      }

      award.tc.get.totalAwardAmount shouldBe 5282.56
      award.tc.get.taxYears.tail.head.taxYearAwardProRataAmount shouldBe 1400.66
      award.tc.get.totalAwardProRataAmount shouldBe 2846.64
      award.tc.get.houseHoldAdviceAmount shouldBe 0.00

      incomeAdvice.tc.get.houseHoldAdviceAmount shouldBe 69254.20
      incomeAdvice.tc.get.taxYears.head.taxYearAdviceAmount shouldBe 29691.63
      incomeAdvice.tc.get.taxYears.tail.head.taxYearAdviceAmount shouldBe 39562.57 //11820.95
      incomeAdvice.tc.get.totalHouseHoldAdviceProRataAmount shouldBe 44135.11
      incomeAdvice.tc.get.totalAwardAmount shouldBe 0.00
    }

  }
}
