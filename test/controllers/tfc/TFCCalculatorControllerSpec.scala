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

package controllers.tfc

import calculators.TFCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.tfc.TFCCalculatorInput
import models.output.tfc.{TFCCalculatorOutput, TFCContribution}
import org.mockito.ArgumentMatchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.i18n.Messages.Implicits._
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents
import utils.FakeCCCalculatorApplication

import scala.concurrent.Future

class TFCCalculatorControllerSpec extends FakeCCCalculatorApplication with MockitoSugar {

  implicit val request = FakeRequest()

  val mockTFCCalculation = TFCCalculatorOutput(householdContribution = TFCContribution(), numberOfPeriods = 1, periods = List())

  "TFCCalculatorController" should {

    "not return NOT_FOUND (calculate) endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-calculator/tax-free-childcare/calculate"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "return Internal Server Error with error message if an exception is thrown during calculation " in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/calculator_input_test.json")
      val inputJson: JsValue = Json.parse(resource.toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[TFCCalculatorInput]

      when(controller.calculator.award(mockEq(JsonResult.get))).thenReturn(Future.failed(new Exception("Something bad happened")))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      val outputJSON = Json.parse(
        """
          |{
          |    "status": 500,
          |    "error": "Something bad happened"
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }


    "Valid JSON at /tax-free-childcare/calculate" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/calculator_input_test.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.OK
    }

    "Accept invalid JSON at /tfc/calculate and return a BadRequest with an error (0 TFC Periods)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/no_period.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/periods",
          |         "validationErrors":[
          |            {
          |               "message":"Please provide at least 1 Period",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /tax-free-childcare/calculate and return a BadRequest with an error (negative value in childcare cost)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/negative_childcareCost.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/periods(0)/children(0)/childcareCost",
          |         "validationErrors":[
          |            {
          |               "message":"Childcare Spend cost should not be less than 0.00",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /tfc/calculate and return a BadRequest with an error (no children)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/no_children.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
      {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/periods(0)/children",
          |         "validationErrors":[
          |            {
          |               "message":"Please provide at least 1 child or maximun of 25 children",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /tax-free-childcare/calculate and return a BadRequest with an error (date missing)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/date_missing.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/until",
          |         "validationErrors":[
          |            {
          |               "message":"error.path.missing",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /tax-free-childcare/calculate and return a BadRequest with an error (Invalid data type)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/invalid_data_type.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/until",
          |         "validationErrors":[
          |            {
          |               "message":"error.expected.jodadate.format",
          |               "args":["yyyy-MM-dd"]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Valid JSON at /tax-free-childcare/calculate(child from and until date is null)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/child_from_until_date_null.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.OK
    }

    "Invalid JSON at /tax-free-childcare/calculate(no disability field)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/no_disability.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

    "Valid JSON at /tax-free-childcare/calculate(TFC Award Calculation Wire up)" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/wire_up_flow_through.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.OK
    }

    "Accept invalid JSON at /tax-free-childcare/calculate(TFC Award Calculation Wire up - wrong scheme(esc))" in {

      val mockTFCCalculatorController = new TFCCalculatorController(applicationMessagesApi) {
        override val calculator = mock[TFCCalculator]
        override val auditEvent = mock[AuditEvents]
      }

      val controller = mockTFCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/incorrect_scheme_name.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[TFCCalculatorInput]())).thenReturn(Future.successful(mockTFCCalculation))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST
    }

  }
}
