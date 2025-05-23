/*
 * Copyright 2024 HM Revenue & Customs
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

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import play.api.libs.json._

class JSONFactorySpec extends FakeCCCalculatorApplication {

  "JSONFactory" must {

    "Return a valid output JSON when error sequence and status are passed" in {
      val status                                         = 400
      val JSONPath                                       = JsPath \ "tfc"
      val validationError                                = JsonValidationError("Very Bad Thing Happened", 400)
      val errorTuple: (JsPath, Seq[JsonValidationError]) = (JSONPath, Seq(validationError))

      val outputJSON = Json.parse("""
                                    |{
                                    |"status": 400,
                                    |"errors":
                                    |[
                                    |   {
                                    |     "path" : "/tfc",
                                    |     "validationErrors" :
                                    |     [
                                    |       {
                                    |        "message": "Very Bad Thing Happened",
                                    |        "args": [400]
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
      val outputJSON = Json.parse("""
                                    |{
                                    |"status": 500,
                                    |"errors": ["Error while generating JSON response"]
                                    | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Left(Nil))
      result shouldBe outputJSON
    }

    "Return a valid output JSON when exception and status are passed" in {
      val status    = 500
      val exception = new Exception("Very Bad Thing Happened")

      val outputJSON = Json.parse("""
                                    |{
                                    |"status": 500,
                                    |"error": "Very Bad Thing Happened"
                                    | }
        """.stripMargin)

      val result = utils.JSONFactory.generateErrorJSON(status, Right(exception))
      result shouldBe outputJSON
    }

//    "Return a valid JSON response with calculation result (Scenario 51 input)" in {
//      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
//      val json: JsValue = Json.parse(resource.toString)
//      val result = json.validate[TCCalculatorInput]
//      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
//      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
//      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
//      val secondPeriodFrom = LocalDate.parse("2016-12-12", formatter)
//      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)
//
//      val outputJson = Json.parse(
//        s"""
//          |{
//          |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//          |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//          |   "totalAwardAmount": 2982.17,
//          |   "houseHoldAdviceAmount": 0.00,
//          |   "taxYears": [
//          |   {
//              |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//              |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//              |   "taxYearAwardAmount": 2982.17,
//              |   "taxYearAdviceAmount": 0.00,
//              |   "periods": [
//              |     {
//              |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//              |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
//              |       "periodNetAmount": 2982.17,
//             |       "periodAdviceAmount": 0.00,
//             |       "elements": {
//             |           "wtcWorkElement": {
//             |             "netAmount": 92.27,
//             |             "maximumAmount": 995.60,
//             |             "taperAmount": 903.33
//             |           },
//             |           "wtcChildcareElement": {
//             |             "netAmount": 699.58,
//             |             "maximumAmount": 699.58,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcIndividualElement": {
//             |             "netAmount": 2077.08,
//             |             "maximumAmount": 2077.08,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcFamilyElement": {
//             |             "netAmount": 113.24,
//             |             "maximumAmount": 113.24,
//             |             "taperAmount": 0.00
//             |           }
//             |         }
//             |     },
//             |     {
//             |       "from": "${secondPeriodFrom.toString("yyyy-MM-dd")}",
//             |       "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//             |       "periodNetAmount": 0.00,
//             |       "periodAdviceAmount": 0.00,
//             |       "elements": {
//             |         "wtcWorkElement": {
//             |             "netAmount": 0.00,
//             |             "maximumAmount":  872.85,
//             |             "taperAmount":  872.85
//             |           },
//             |           "wtcChildcareElement": {
//             |             "netAmount": 0.00,
//             |             "maximumAmount": 0.00,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcIndividualElement": {
//             |             "netAmount": 0.00,
//             |             "maximumAmount": 0.00,
//             |             "taperAmount": 0.00
//             |             },
//             |           "ctcFamilyElement": {
//             |             "maximumAmount": 0.00,
//             |             "netAmount": 0.00,
//             |             "taperAmount": 0.00
//             |           }
//             |         }
//             |       }
//              |   ]
//           |   }
//          |   ]
//          |}
//        """.stripMargin)
//
//      result match {
//        case JsSuccess(x, _) =>
//          val setup = await(TCCalculator.award(x))
//          Json.toJson(setup) shouldBe outputJson
//        case _ => throw new Exception
//      }
//    }
//
//    "Return a valid JSON response with advice earnings calculation result (Scenario 51 input)" in {
//      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
//      val json: JsValue = Json.parse(resource.toString)
//      val result = json.validate[TCCalculatorInput]
//      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
//      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
//      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
//      val secondPeriodFrom = LocalDate.parse("2016-12-12", formatter)
//      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)
//
//      val outputJson = Json.parse(
//        s"""
//          |{
//          |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//          |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//          |   "totalAwardAmount": 0.00,
//          |   "houseHoldAdviceAmount": 14970.064,
//          |   "taxYears": [
//          |   {
//              |   "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//              |   "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//              |   "taxYearAwardAmount": 0.00,
//              |   "taxYearAdviceAmount": 14970.064,
//              |   "periods": [
//              |     {
//              |      "from": "${firstPeriodFrom.toString("yyyy-MM-dd")}",
//              |      "until": "${firstPeriodTo.toString("yyyy-MM-dd")}",
//              |      "periodNetAmount": 0.00,
//             |       "periodAdviceAmount": 10817.46,
//             |       "elements": {
//             |           "wtcWorkElement": {
//             |             "netAmount": 995.60,
//             |             "maximumAmount": 995.60,
//             |             "taperAmount": 0.00
//             |           },
//             |           "wtcChildcareElement": {
//             |             "netAmount": 699.58,
//             |             "maximumAmount": 699.58,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcIndividualElement": {
//             |             "netAmount": 2077.08,
//             |             "maximumAmount": 2077.08,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcFamilyElement": {
//             |             "netAmount": 113.24,
//             |             "maximumAmount": 113.24,
//             |             "taperAmount": 0.00
//             |           }
//             |         }
//             |     },
//             |     {
//             |       "from": "${secondPeriodFrom.toString("yyyy-MM-dd")}",
//             |       "until": "${secondPeriodTo.toString("yyyy-MM-dd")}",
//             |       "periodNetAmount": 0.00,
//             |       "periodAdviceAmount": 4152.604,
//             |       "elements": {
//             |         "wtcWorkElement": {
//             |             "netAmount": 872.85,
//             |             "maximumAmount": 872.85,
//             |             "taperAmount": 0.00
//             |           },
//             |           "wtcChildcareElement": {
//             |             "netAmount": 0.00,
//             |             "maximumAmount": 0.00,
//             |             "taperAmount": 0.00
//             |           },
//             |           "ctcIndividualElement": {
//             |             "netAmount": 0.00,
//             |             "maximumAmount": 0.00,
//             |             "taperAmount": 0.00
//             |             },
//             |           "ctcFamilyElement": {
//             |             "maximumAmount": 0.00,
//             |             "netAmount": 0.00,
//             |             "taperAmount": 0.00
//             |           }
//             |         }
//             |       }
//              |   ]
//           |   }
//          |   ]
//          | }
//        """.stripMargin)
//
//
//      result match {
//        case JsSuccess(x, _) =>
//          val setup = await(TCCalculator.incomeAdvice(x))
//          Json.toJson(setup) shouldBe outputJson
//        case _ => throw new Exception
//      }
//    }
  }

}
