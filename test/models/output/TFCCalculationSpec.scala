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

package models.output

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json
import uk.gov.hmrc.play.test.UnitSpec
import utils.FakeCCCalculatorApplication

class TFCCalculationSpec extends UnitSpec with FakeCCCalculatorApplication {

  "TFCCalculation models" should {

    "TFCCalculation should write to JSON" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse ("2016-08-01",formatter)
      val until = LocalDate.parse ("2016-11-01",formatter)
      val numberOfPeriods = 0

      val contribution = models.output.tfc.Contribution(
        BigDecimal(0.00),
        BigDecimal(0.00),
        BigDecimal(0.00)
      )

      val outputChild = models.output.tfc.OutputChild(
        childCareCost =  BigDecimal(0.00),
        childContribution = contribution
      )

      val tfcPeriod = models.output.tfc.TFCPeriod(
        from = from,
        until = until,
        periodContribution = contribution,
        children = List(outputChild)
      )

      val tfcCalculation = models.output.tfc.TFCCalculation(
        householdContribution = contribution,
        numberOfPeriods = numberOfPeriods.toShort,
        periods = List(tfcPeriod)
      )

      val json = Json.toJson[models.output.tfc.TFCCalculation](tfcCalculation)

      json shouldBe Json.parse(
        """
          |{
          |   "householdContribution": {
          |			  "parent": 0.0,
          |			  "government": 0.0,
          |			  "totalChildCareSpend": 0.0
          |		},
          |   "numberOfPeriods": 0,
          |	  "periods": [
          |   {
          |		  "from": "2016-08-01",
          |		  "until": "2016-11-01",
          |		  "periodContribution": {
          |			  "parent": 0.0,
          |			  "government": 0.0,
          |			  "totalChildCareSpend": 0.0
          |		},
          |		"children": [
          |   {
          |			  "childCareCost": 0.0,
          |			  "childContribution": {
          |				"parent": 0.0,
          |				"government": 0.0,
          |				"totalChildCareSpend": 0.0
          |			}
          |		}
          |  ]
          |	}
          | ]
          |}
        """.stripMargin)
    }
  }
}
