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

import calculators.ESCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.esc.ESCCalculatorInput
import models.output.esc.ESCCalculation
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.FakeCCCalculatorApplication
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class ESCScenarioSpec extends UnitSpec with FakeCCCalculatorApplication {

  "ESCScenarioSpec" should {
    val testData = Table(
      ("Tax year", "Scenario number"),
      ("2016/2017", 1),
      ("2016/2017", 2),
      ("2016/2017", 3),
      ("2016/2017", 4),
      ("2016/2017", 5),
      ("2016/2017", 6),
      ("2016/2017", 7),
      ("2016/2017", 8),
      ("2016/2017", 9),
      ("2016/2017", 10),
      ("2016/2017", 11),
      ("2016/2017", 12),
      ("2016/2017", 13),
      ("2016/2017", 14),
      ("2016/2017", 15),
      ("2016/2017", 16),
      ("2016/2017", 17),
      ("2016/2017", 18),
      ("2016/2017", 19),
      ("2016/2017", 20),
      ("2016/2017", 21),
      ("2016/2017", 22),
      ("2016/2017", 23),
      ("2016/2017", 24),
      ("2016/2017", 25),
      ("2016/2017", 26),
      ("2016/2017", 27),
      ("2016/2017", 28),
      ("2017/2018", 29),
      ("2016/2017", 30),
      ("2017/2018", 31),
      ("2017/2018", 32),
      ("2017/2018", 33),
      ("2017/2018", 34)
    )

    forAll(testData) { case (taxYear, scenarioNumber) =>

      s"(TY ${taxYear} Scenario ${scenarioNumber}) Generate total award with claimants" in {
        val resource: JsonNode = JsonLoader.fromResource(s"/json/esc/input/scenario_${scenarioNumber}.json")
        val json: JsValue = Json.parse(resource.toString)
        val inputJson = json.validate[ESCCalculatorInput]
        inputJson.isInstanceOf[JsSuccess[ESCCalculatorInput]] shouldBe true

        val result: ESCCalculation = ESCCalculator.calculator.award(inputJson.get)
        val resourceJson = JsonLoader.fromResource(s"/json/esc/output/scenario_${scenarioNumber}.json")
        val outputJson: JsValue = Json.parse(resourceJson.toString)
        Json.toJson[ESCCalculation](result) shouldBe outputJson
      }
    }

  }

}
