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

import org.scalatest.matchers.should.Matchers.shouldBe
import play.api.libs.json.*

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
  }

}
