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

import models.output.esc.{ESCTaxAndNi, ESCSavings, ESCIncome}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, Periods}

class ESCCalculatorOutputSpec extends UnitSpec with FakeCCCalculatorApplication {

  "ESCCalculation models" should {

    "ESCCalculation should write to JSON" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val from = LocalDate.parse ("2016-08-27",formatter)
      val until = LocalDate.parse ("2016-05-06",formatter)

      val claimant = models.output.esc.ESCClaimant(
        eligibleMonthsInTaxYear = 0,
        income = ESCIncome(),
        vouchers = false,
        escStartDate = from,
        escAmountPeriod = Periods.Monthly,
        savings = ESCSavings(),
        taxAndNIAfterSacrifice = ESCTaxAndNi(),
        taxAndNIBeforeSacrifice = ESCTaxAndNi()
      )

      val taxYear = models.output.esc.ESCTaxYear(
        from = from,
        until = until,
        totalSavings = ESCSavings(),
        claimants = List(claimant)
      )

      val outputCalculation = models.output.esc.ESCCalculatorOutput(
        from = from,
        until = until,
        totalSavings = ESCSavings(),
        taxYears = List(taxYear)
      )

      val json = Json.toJson[models.output.esc.ESCCalculatorOutput](outputCalculation)
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
          |               "vouchers":false,
          |               "escAmount":0.0,
          |               "escAmountPeriod":"Month",
          |               "escStartDate":"2016-08-27",
          |               "savings":{
          |                  "totalSaving":0.0,
          |                  "taxSaving":0.0,
          |                  "niSaving":0.0
          |               },
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
}
