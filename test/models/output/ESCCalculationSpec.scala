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

import models.output.esc.{TaxAndNI, Savings, Income}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.Json
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, Periods}


class ESCCalculationSpec extends UnitSpec with FakeCCCalculatorApplication {

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
}
