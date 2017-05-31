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

package models.input

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.input.esc.ESCEligibility
import models.input.tc.TCEligibility
import models.input.tfc.TFCEligibility
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger}

import scala.util.{Failure, Success}

/**
 * Created by adamconder on 07/07/15.
 */
class APIModelSpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger {

  "APIModel" should {

    "return Tax Credits eligibility result" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result match {
        case JsSuccess(x, _) =>
          x.getTaxCreditsEligibility.isInstanceOf[Success[TCEligibility]] shouldBe true
        case _ => throw new Exception
      }
    }

    "return Failure if Tax Credits eligibility result does not exist" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/no_eligiblity.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result match {
        case JsSuccess(x, _) =>
          x.getTaxCreditsEligibility.isInstanceOf[Failure[TCEligibility]] shouldBe true
        case _ => throw new Exception
      }
    }

    "return TFC eligibility result" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/tfc_eligible.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result match {
        case JsSuccess(x, _) =>
          x.getTFCEligibility.isInstanceOf[Success[TFCEligibility]] shouldBe true
        case _ => throw new Exception
      }
    }

    "return Failure if TFC eligibility result does not exist" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/no_eligiblity.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      logResult(result)
      result match {
        case JsSuccess(x, _) =>
          x.getTFCEligibility.isInstanceOf[Failure[TFCEligibility]] shouldBe true
        case _ => throw new Exception
      }
    }

  }
}
