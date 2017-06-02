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

import calculators.TFCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.tfc.TFCEligibility
import models.output.tfc.TFCCalculation
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.FakeCCCalculatorApplication
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table

class TFCScenarioSpec extends UnitSpec with FakeCCCalculatorApplication {

  "TFCScenarioSpec" should {

    val testCases = Table(
      ("Description", "Scenario Number"),
      ("Generate total award Single claimant, one child and 1 period", "scenario_01"),
      ("Generate total award Single claimant, one child and 1 period", "scenario_02"),
      ("Generate total award Single claimant, one child who is disabled and 1 period", "scenario_03"),
      ("Generate total award Single claimant, one child and one yet to be born and 1 period", "scenario_04"),
      ("Generate total award couple, one child and 1 period", "scenario_05"),
      ("Generate total award couple, one child who is disabled and one yet to be born and 1 period", "scenario_06"),
      ("Generate total award couple, two children one disabled and 1 period", "scenario_07"),
      ("Generate total award couple, two children both disabled and 1 period", "scenario_08"),
      ("Generate total award couple, two children both disabled, one yet to be born and 1 period", "scenario_09"),
      ("Generate total award single claimant, one child and 4 periods", "scenario_10"),
      ("Generate total award claimant and partner, two children and 4 periods", "scenario_11"),
      ("Generate total award Single claimant, two children among one of them disabled and 4 periods", "scenario_12"),
      ("Generate total award claimant with partner, single child and he turns 11 on 1st sept with 4 periods", "scenario_13"),
      ("Generate total award claimant, 2 children one of them turns 11 and other one has sept 1st day with 4 periods", "scenario_14"),
      ("Generate total award claimant with partner, 2 childs are disabled with 4 periods", "scenario_15"),
      ("Generate total award claimant with partner, 2 childs one of them born in 4th period with 4 periods", "scenario_16"),
      ("Generate total award claimant with partner, one of child disabled and other one born in period 3 with 4 periods", "scenario_17"),
      ("Generate total award Single claimant, two children among one of them disabled and 4 periods", "scenario_18"),
      ("Generate total award Single claimant, two children among one of them disabled and 4 periods", "scenario_19"),
      ("Generate total award Single claimant, two children among one of them disabled and 4 periods", "scenario_20"),
      ("Generate total award Single claimant, two children among one of them disabled and 8 periods", "scenario_21"),
      ("Generate total award Single claimant, two children among one of them disabled and 6 periods", "scenario_22"),
      ("Generate total award for 2 qualifying children in 6 periods with one child disabled", "scenario_23"),
      ("Generate total award single claimant, single child", "scenario_24"),
      ("Generate total award, 3 children, 2 dropping out in different years, child born 7th period", "scenario_25"),
      ("Generate total award, 4 children, twin born in 3rd period, adoption in 6th period", "scenario_26"),
      ("Generate total award, 3 children, twin born on either side of the TFC period", "scenario_27")
    )

    forAll(testCases) { case (description, scenario) =>
      s"(Scenario ${scenario}) ${description}" in {
        val resource: JsonNode = JsonLoader.fromResource(s"/json/tfc/input/${scenario}.json")
        val json: JsValue = Json.parse(resource.toString)
        val inputJson = json.validate[TFCEligibility]
        inputJson.isInstanceOf[JsSuccess[TFCEligibility]] shouldBe true

        val result: TFCCalculation = TFCCalculator.calculator.award(inputJson.get)
        val resourceJson = JsonLoader.fromResource(s"/json/tfc/output/${scenario}.json")
        val outputJson: JsValue = Json.parse(resourceJson.toString)

        Json.toJson(result) shouldBe outputJson
      }
    }
  }
}
