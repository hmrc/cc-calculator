/*
 * Copyright 2019 HM Revenue & Customs
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

package controllers.tc

import calculators.TCCalculator
import com.github.fge.jackson.JsonLoader
import models.input.tc.TCCalculatorInput
import models.output.tc.TCCalculatorOutput
import org.joda.time.LocalDate
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents
import utils.FakeCCCalculatorApplication

import scala.concurrent.Future

class TaxCreditCalculatorControllerSpec extends FakeCCCalculatorApplication with MockitoSugar with BeforeAndAfterEach {

  val validInput: JsValue  = Json.parse(JsonLoader.fromResource("/json/tc/input/valid_json.json").toString)
  val validTCOutput: TCCalculatorOutput = TCCalculatorOutput(LocalDate.now(), LocalDate.now(), 0, 0, Nil)

  val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json")
  val validRequest: FakeRequest[JsValue] = request.withBody(validInput)

  val invalidData = Table(
    ("Json", "Path", "Error", "Args"),
    ("empty_json", "/taxYears", "error.path.missing", ""),
    ("invalid_date", "/taxYears(0)/from", "error.expected.jodadate.format", "\"\""),
    ("negative_childcare_cost", "/taxYears(0)/periods(0)/children(0)/childcareCost", "Childcare Spend cost should not be less than 0.00", "")
  )

  lazy val audits = app.injector.instanceOf[AuditEvents]
  lazy val tcc = app.injector.instanceOf[TCCalculator]


  "when calculate is called" should {

    "not return NOT_FOUND for this endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-calculator/tax-credits/calculate/total-award"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "return status OK and the response of calculator.incomeAdvice if valid request is given" in {
      val SUT = new TaxCreditCalculatorController(mcc, audits, tcc) {
        override val calculator =  mock[TCCalculator]
        override val auditEvent = mock[AuditEvents]
      }
      when(
        SUT.calculator.award(any[TCCalculatorInput]())
      ).thenReturn(
        Future.successful(validTCOutput)
      )
      val result = await(SUT.calculate()(validRequest))
      status(result) shouldBe OK
      jsonBodyOf(result) shouldBe Json.toJson(validTCOutput)
    }

    "return INTERNAL_SERVER_ERROR and error message if calculator.incomeAdvice throws exception" in {
      val SUT = new TaxCreditCalculatorController(mcc, audits, tcc) {
        override val calculator =  mock[TCCalculator]
        override val auditEvent = mock[AuditEvents]
      }
      when(
        SUT.calculator.award(any[TCCalculatorInput]())
      ).thenReturn(
        Future.failed(new Exception("Something bad happened"))
      )
      val result = await(SUT.calculate()(validRequest))
      status(result) shouldBe INTERNAL_SERVER_ERROR
      jsonBodyOf(result) shouldBe Json.parse(
        """
          |{
          |    "status": 500,
          |    "error": "Something bad happened"
          |}
        """.stripMargin)
    }

    "return BAD_REQUEST" when {

      forAll(invalidData) { case (invalidJson, path, error, args) =>

        s"$invalidJson is given and return error: '$error' for path: '$path'" in {
          val SUT = new TaxCreditCalculatorController(mcc, audits, tcc) {
            override val calculator =  mock[TCCalculator]
            override val auditEvent = mock[AuditEvents]
          }
          val json = Json.parse(JsonLoader.fromResource(s"/json/tc/input/$invalidJson.json").toString)
          val invalidRequest: FakeRequest[JsValue] = request.withBody(json)

          when(
            SUT.calculator.award(any[TCCalculatorInput]())
          ).thenReturn(
            Future.successful(validTCOutput)
          )
          val result = await(SUT.calculate()(invalidRequest))
          status(result) shouldBe BAD_REQUEST
          jsonBodyOf(result) shouldBe Json.parse(
            s"""
               |{
               |    "status": 400,
               |    "errors": [
               |    {
               |       "path": "$path",
               |       "validationErrors": [
               |       {
               |         "message": "$error",
               |         "args": [$args]
               |       }
               |       ]
               |    }
               |    ]
               |}
            """.stripMargin)
        }
      }
    }

  }

}
