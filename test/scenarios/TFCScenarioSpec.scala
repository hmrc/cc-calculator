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

package test

import calculators.TFCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger, JSONFactory}


class TFCScenarioSpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger with ValidateCalculations {

  "TFCScenarioSpec" should {

    "(Scenario 1) Generate total award Single claimant, one child and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_01.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_01.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 2) Generate total award Single claimant, one child and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_02.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_02.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 3) Generate total award Single claimant, one child who is disabled and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_03.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_03.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 4) Generate total award Single claimant, one child and one yet to be born and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_04.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_04.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson

    }

    "(Scenario 5) Generate total award couple, one child and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_05.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_05.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 6) Generate total award couple, one child who is disabled and one yet to be born and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_06.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_06.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 7) Generate total award couple, two children one disabled and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_07.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_07.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 8) Generate total award couple, two children both disabled and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_08.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_08.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 9) Generate total award couple, two children both disabled, one yet to be born and 1 period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_09.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_09.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 10) Generate total award single claimant, one child and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_10.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_10.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }


    "(Scenario 11) Generate total award claimant and partner, two children and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_11.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_11.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 12) Generate total award Single claimant, two children among one of them disabled and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_12.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)
      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_12.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 13) Generate total award claimant with partner, single child and he turns 11 on 1st sept with 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_13.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_13.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 14) Generate total award claimant, 2 childs one of them turns 11 and other one has sept 1st day with 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_14.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_14.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 15) Generate total award claimant with partner, 2 childs are disabled with 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_15.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_15.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson


    }

    "(Scenario 16) Generate total award claimant with partner, 2 childs one of them born in 4th period with 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_16.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_16.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 17) Generate total award claimant with partner, one of child disabled and other one born in period 3 with 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_17.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_17.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 18) Generate total award Single claimant, two children among one of them disabled and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_18.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 19) Generate total award Single claimant, two children among one of them disabled and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_19.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_19.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 20) Generate total award Single claimant, two children among one of them disabled and 4 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_20.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_20.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 21) Generate total award Single claimant, two children among one of them disabled and 8 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_21.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_21.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 22) Generate total award Single claimant, two children among one of them disabled and 6 periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_22.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_22.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 23) Generate total award for 2 qualifying children in 6 periods with one child disabled" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_23.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_23.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 24) Generate total award single claimant, single child" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_24.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_24.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 25) Generate total award, 3 children, 2 dropping out in different years, child born 7th period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_25.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_25.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 26) Generate total award, 4 children, twin born in 3rd period, adoption in 6th period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_26.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_26.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }

    "(Scenario 27) Generate total award, 3 children, twin born on either side of the TFC period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/scenario_27.json")
      val json: JsValue = Json.parse(resource.toString)
      val inputJson = json.validate[Request]
      inputJson.isInstanceOf[JsSuccess[Request]] shouldBe true

      val result: AwardPeriod = TFCCalculator.calculator.award(inputJson.get)

      val resourceJson = JsonLoader.fromResource("/json/tfc/output/scenario_27.json")
      val outputJson: JsValue = Json.parse(resourceJson.toString)

      JSONFactory.generateResultJson(result) shouldBe outputJson
    }
  }
}
