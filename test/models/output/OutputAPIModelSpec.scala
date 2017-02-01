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

import models.output.OutputAPIModel.{AwardPeriod, Response}
import models.output.esc.{Income, Savings, TaxAndNI}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger, Periods}

class OutputAPIModelSpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger {

  "APIModelSpec" should {

    "Response model's write to json" in {
      val response = Response(AwardPeriod())
      val js = Json.toJson(response)
      js shouldBe Json.parse("""{"awardPeriod":{"tc":null,"tfc":null,"esc":null}}""")
    }

    "Total Award model's write to json" in {
      val response = AwardPeriod()
      val js = Json.toJson[AwardPeriod](response)
      js shouldBe Json.parse("""{"tc":null,"tfc":null,"esc":null}""")
    }
  }

  "ESCCalculation models" should {

    "ESCCalculation should write to JSON" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse ("2016-08-27",formatter)
      val until = LocalDate.parse ("2016-05-06",formatter)

      val claimant = models.output.esc.Claimant(
        eligibleMonthsInTaxYear = 0,
        income = Income(),
        elements = models.output.esc.ClaimantElements(),
        escStartDate = from,
        escAmountPeriod = Periods.Monthly,
        savings = Savings(),
        maximumRelief = BigDecimal(0.00),
        maximumReliefPeriod = Periods.Monthly,
        taxAndNIAfterSacrifice = TaxAndNI(),
        taxAndNIBeforeSacrifice = TaxAndNI()
      )

      val taxYear = models.output.esc.TaxYear(
        from = from,
        until = until,
        totalSavings = Savings(),
        claimants = List(claimant)
      )

      val outputCalculation = models.output.esc.ESCCalculation(
        from = from,
        until = until,
        totalSavings = Savings(),
        taxYears = List(taxYear)
      )

      val json = Json.toJson[models.output.esc.ESCCalculation](outputCalculation)
      json shouldBe Json.parse(
        """
         {
          |   "from":"2016-08-27",
          |   "until":"2016-05-06",
          |   "totalSavings":{
          |      "totalSaving":0.0,
          |      "taxSaving":0.0,
          |      "niSaving":0.0
          |   },
          |   "taxYears":[
          |      {
          |         "from":"2016-08-27",
          |         "until":"2016-05-06",
          |         "totalSavings":{
          |            "totalSaving":0.0,
          |            "taxSaving":0.0,
          |            "niSaving":0.0
          |         },
          |         "claimants":[
          |            {
          |               "qualifying":false,
          |               "eligibleMonthsInTaxYear":0,
          |               "isPartner":false,
          |               "income":{
          |                  "taxablePay":0.0,
          |                  "gross":0.0,
          |                  "taxCode":"",
          |                  "niCategory":""
          |               },
          |               "elements":{
          |                  "vouchers":false
          |               },
          |               "escAmount":0.0,
          |               "escAmountPeriod":"Month",
          |               "escStartDate":"2016-08-27",
          |               "savings":{
          |                  "totalSaving":0.0,
          |                  "taxSaving":0.0,
          |                  "niSaving":0.0
          |               },
          |               "maximumRelief":0.0,
          |               "maximumReliefPeriod":"Month",
          |               "taxAndNIBeforeSacrifice":{
          |                  "taxPaid":0.0,
          |                  "niPaid":0.0
          |               },
          |               "taxAndNIAfterSacrifice":{
          |                  "taxPaid":0.0,
          |                  "niPaid":0.0
          |               }
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)
    }
  }

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
        id = 0,
        name = Some("Child 1"),
        childCareCost =  BigDecimal(0.00),
        childContribution = contribution,
        timeToMaximizeTopUp = 0,
        failures = List()
      )

      val tfcPeriod = models.output.tfc.TFCPeriod(
        from = from,
        until = until,
        periodContribution = contribution,
        children = List(outputChild)
      )

      val tfcCalculation = models.output.tfc.TFCCalculation(
        from = from,
        until = until,
        householdContribution = contribution,
        numberOfPeriods = numberOfPeriods.toShort,
        periods = List(tfcPeriod)
      )

      val json = Json.toJson[models.output.tfc.TFCCalculation](tfcCalculation)

      json shouldBe Json.parse(
        """
          |{
          |	  "from": "2016-08-01",
          |	  "until": "2016-11-01",
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
          |			  "id": 0,
          |			  "name": "Child 1",
          |			  "childCareCost": 0.0,
          |			  "childContribution": {
          |				"parent": 0.0,
          |				"government": 0.0,
          |				"totalChildCareSpend": 0.0
          |			},
          |			"timeToMaximizeTopUp": 0,
          |			"failures": []
          |		}
          |  ]
          |	}
          | ]
          |}
        """.stripMargin)
    }
  }

}
