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

package scenarios

import calculators.ESCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger, JSONFactory}

class ESCScenarioSpec extends UnitSpec with CCJsonLogger with FakeCCCalculatorApplication{
  "ESCScenarioSpec" should {

    "(TY 2016/2017 Scenario 1) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_1.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 2) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_2.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_2.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 3) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_3.json")
      val outputJson:  JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 4) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_4.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_4.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 5) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_5.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_5.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 6) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_6.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_6.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 7) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_7.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_7.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 8) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_8.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_8.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 9) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_9.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_9.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 10) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_10.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_10.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 11) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_11.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_11.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 12) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_12.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_12.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 13) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_13.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_13.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 14) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_14.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_14.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 15) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_15.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_15.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 16) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_16.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_16.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 17) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_17.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_17.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 18) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_18.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 19) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_19.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_19.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 20) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_20.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_20.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 21) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_21.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_21.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 22) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_22.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_22.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 23) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_23.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_23.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 24) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_24.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_24.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 25) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_25.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_25.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 26) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_26.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_26.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 27) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_27.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_27.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 28) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_28.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_28.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2017/2018 Scenario 29) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_29.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_29.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2016/2017 Scenario 30) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_30.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_30.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2017/2018 Scenario 31) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_31.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_31.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2017/2018 Scenario 32) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_32.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_32.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2017/2018 Scenario 33) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_33.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_33.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(TY 2017/2018 Scenario 34) Generate total award with claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/scenario_34.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result : AwardPeriod = ESCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/esc/output/scenario_34.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

     JSONFactory.generateResultJson(result) shouldBe outputJson
    }



  }
}
