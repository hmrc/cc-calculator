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

package utils

import calculators.{TCCalculator, TFCCalculator}
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import models.output.tc.TCCalculation
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.libs.json._

class JSONFactorySpec extends FakeCCCalculatorApplication {

  "JSONFactory" should {

    "Return a valid output JSON when error sequence and status are passed" in {
      val status = 400
      val JSONPath = JsPath \ "tc"
      val validationError = ValidationError("Very Bad Thing Happened")
      val errorTuple: (play.api.libs.json.JsPath, Seq[play.api.data.validation.ValidationError]) = (JSONPath, Seq(validationError))

      val outputJSON = Json.parse(
        """
          |{
          |"status": 400,
          |"errors":
          |[
          |   {
          |     "path" : "/tc",
          |     "validationErrors" :
          |     [
          |       {
          |        "message": "Very Bad Thing Happened",
          |        "args": []
          |       }
          |     ]
          |   }
          | ]
          | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Left(Seq(errorTuple)))
      result shouldBe outputJSON
    }

    "Return a valid output JSON if error sequence is missing" in {
      val status = 500
      val outputJSON = Json.parse(
        """
          |{
          |"status": 500,
          |"errors": ["Error while generating JSON response"]
          | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Left(Nil))
      result shouldBe outputJSON
    }

    "Return a valid output JSON when exception and status are passed" in {
      val status = 500
      val exception = new Exception("Very Bad Thing Happened")

      val outputJSON = Json.parse(
        """
          |{
          |"status": 500,
          |"error": "Very Bad Thing Happened"
          | }
        """.stripMargin)

      val result  = utils.JSONFactory.generateErrorJSON(status, Right(exception))
      result shouldBe outputJSON
    }

    "Return a valid response with calculation result" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
      val calculation = TCCalculation(
                      from = firstPeriodFrom,
                      until = firstPeriodTo,
                      totalAwardAmount = 5000.00,
                      houseHoldAdviceAmount = 0.00,
                      taxYears = List())

      val response = AwardPeriod(tc = Some(calculation))
      val outputJson = Json.parse(
        s"""
          |{
          |"calculation": {
          | "tc": {
          |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
          |   "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
          |   "totalAwardAmount": 5000.00,
          |    "totalAwardProRataAmount": 0.00,
          |   "houseHoldAdviceAmount": 0.00,
          |    "totalHouseHoldAdviceProRataAmount": 0.00,
          |   "taxYears": []
          | },
          | "tfc": null,
          | "esc": null
          |}
          |}
        """.stripMargin)

      val result = utils.JSONFactory.generateResultJson(response)
      result shouldBe outputJson
    }

    "Return a valid JSON response with calculation result (Scenario 51 input)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodFrom = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      val outputJson = Json.parse(
        s"""
          |{
          |"calculation": {
          | "tc": {
          |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
          |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
          |   "totalAwardAmount": 2982.17,
          |   "totalAwardProRataAmount" :0.00,
          |   "houseHoldAdviceAmount": 0.00,
          |  "totalHouseHoldAdviceProRataAmount" :0.00,
          |   "taxYears": [
          |   {
              |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
              |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
              |   "taxYearAwardAmount": 2982.17,
              |    "taxYearAwardProRataAmount" : 0.00,
              |   "taxYearAdviceAmount": 0.00,
              |    "taxYearAdviceProRataAmount" : 0.00,
              |   "periods": [
              |     {
              |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
              |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
              |       "periodNetAmount": 2982.17,
             |       "periodAdviceAmount": 0.00,
             |       "elements": {
             |           "wtcWorkElement": {
             |             "netAmount": 92.27,
             |             "maximumAmount": 995.60,
             |             "taperAmount": 903.33
             |           },
             |           "wtcChildcareElement": {
             |             "netAmount": 699.58,
             |             "maximumAmount": 699.58,
             |             "taperAmount": 0.00
             |           },
             |           "ctcIndividualElement": {
             |             "netAmount": 2077.08,
             |             "maximumAmount": 2077.08,
             |             "taperAmount": 0.00
             |           },
             |           "ctcFamilyElement": {
             |             "netAmount": 113.24,
             |             "maximumAmount": 113.24,
             |             "taperAmount": 0.00
             |           }
             |         }
             |     },
             |     {
             |       "from": "${secondPeriodFrom.toString("yyyy-MM-dd")}",
             |       "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
             |       "periodNetAmount": 0.00,
             |       "periodAdviceAmount": 0.00,
             |       "elements": {
             |         "wtcWorkElement": {
             |             "netAmount": 0.00,
             |             "maximumAmount":  872.85,
             |             "taperAmount":  872.85
             |           },
             |           "wtcChildcareElement": {
             |             "netAmount": 0.00,
             |             "maximumAmount": 0.00,
             |             "taperAmount": 0.00
             |           },
             |           "ctcIndividualElement": {
             |             "netAmount": 0.00,
             |             "maximumAmount": 0.00,
             |             "taperAmount": 0.00
             |             },
             |           "ctcFamilyElement": {
             |             "maximumAmount": 0.00,
             |             "netAmount": 0.00,
             |             "taperAmount": 0.00
             |           }
             |         }
             |       }
              |   ]
           |   }
          |   ]
          | },
          | "tfc": null,
          | "esc": null
          |}
          |}
        """.stripMargin)

      result match {
        case JsSuccess(x, _) =>
          val setup = TCCalculator.calculator.award(x)
          val result = utils.JSONFactory.generateResultJson(setup)
          result shouldBe outputJson
        case _ => throw new Exception
      }
    }

    "Return a valid JSON response with advice earnings calculation result (Scenario 51 input)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodFrom = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      val outputJson = Json.parse(
        s"""
          |{
          |"calculation": {
          | "tc": {
          |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
          |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
          |   "totalAwardAmount": 0.00,
          |   "totalAwardProRataAmount" : 0.00,
          |   "houseHoldAdviceAmount": 14970.064,
          |   "totalHouseHoldAdviceProRataAmount" :0.00,
          |   "taxYears": [
          |   {
              |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
              |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
              |   "taxYearAwardAmount": 0.00,
              |   "taxYearAwardProRataAmount" : 0.00,
              |   "taxYearAdviceAmount": 14970.064,
              |   "taxYearAdviceProRataAmount" : 0.00,
              |   "periods": [
              |     {
              |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
              |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
              |      "periodNetAmount": 0.00,
             |       "periodAdviceAmount": 10817.46,
             |       "elements": {
             |           "wtcWorkElement": {
             |             "netAmount": 995.60,
             |             "maximumAmount": 995.60,
             |             "taperAmount": 0.00
             |           },
             |           "wtcChildcareElement": {
             |             "netAmount": 699.58,
             |             "maximumAmount": 699.58,
             |             "taperAmount": 0.00
             |           },
             |           "ctcIndividualElement": {
             |             "netAmount": 2077.08,
             |             "maximumAmount": 2077.08,
             |             "taperAmount": 0.00
             |           },
             |           "ctcFamilyElement": {
             |             "netAmount": 113.24,
             |             "maximumAmount": 113.24,
             |             "taperAmount": 0.00
             |           }
             |         }
             |     },
             |     {
             |       "from": "${secondPeriodFrom.toString("yyyy-MM-dd")}",
             |       "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
             |       "periodNetAmount": 0.00,
             |       "periodAdviceAmount": 4152.604,
             |       "elements": {
             |         "wtcWorkElement": {
             |             "netAmount": 872.85,
             |             "maximumAmount": 872.85,
             |             "taperAmount": 0.00
             |           },
             |           "wtcChildcareElement": {
             |             "netAmount": 0.00,
             |             "maximumAmount": 0.00,
             |             "taperAmount": 0.00
             |           },
             |           "ctcIndividualElement": {
             |             "netAmount": 0.00,
             |             "maximumAmount": 0.00,
             |             "taperAmount": 0.00
             |             },
             |           "ctcFamilyElement": {
             |             "maximumAmount": 0.00,
             |             "netAmount": 0.00,
             |             "taperAmount": 0.00
             |           }
             |         }
             |       }
              |   ]
           |   }
          |   ]
          | },
          | "tfc": null,
          | "esc": null
          |}
          |}
        """.stripMargin)


      result match {
        case JsSuccess(x, _) =>
          val setup = TCCalculator.calculator.incomeAdvice(x)
          val result = utils.JSONFactory.generateResultJson(setup)
          result shouldBe outputJson
        case _ => throw new Exception
      }
    }

    "Return a valid JSON response with award earnings TFC calculation result (calculator_input_test1)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/calculator_input_test1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-08-27", formatter)
      val firstPeriodTo = LocalDate.parse("2016-11-27", formatter)

      val outputJson = Json.parse(
        s"""
           |{
           |  "calculation" : {
           |    "tc": null,
           |    "tfc": {
           |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
           |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
           |      "householdContribution": {
           |        "parent": 8500.00,
           |        "government": 500.00,
           |        "totalChildCareSpend": 9000.00
           |      },
           |      "numberOfPeriods" : 1,
           |      "periods" : [
           |        {
           |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
           |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
           |          "periodContribution": {
           |            "parent": 8500.00,
           |            "government": 500.00,
           |            "totalChildCareSpend": 9000.00
           |          },
           |          "children": [
           |            {
           |              "id": 1,
           |              "name" : "Child 1",
           |              "childCareCost": 3000.00,
           |              "childContribution" : {
           |                "parent": 8500.00,
           |                "government": 500.00,
           |                "totalChildCareSpend": 9000.00
           |              },
           |              "timeToMaximizeTopUp" : 0,
           |              "failures" : []
           |            }
           |          ]
           |        }
           |      ]
           |    },
           |    "esc": null
           |  }
           |}
           |
        """.stripMargin)


      result match {
        case JsSuccess(x, _) =>
          val setup = TFCCalculator.calculator.award(x)
          val result = utils.JSONFactory.generateResultJson(setup)
          result shouldBe outputJson
        case _ => throw new Exception
      }
    }



  }
}
