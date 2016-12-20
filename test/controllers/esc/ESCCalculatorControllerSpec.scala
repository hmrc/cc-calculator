/*
 * Copyright 2016 HM Revenue & Customs
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

package controllers.esc

import akka.stream.ActorMaterializer
import calculators.ESCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import controllers.FakeCCCalculatorApplication
import helper.JsonRequestHelper._
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import org.mockito.Matchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatestplus.play.OneAppPerSuite
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.inject.guice.GuiceApplicationBuilder
import service.AuditEvents
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import play.api.Play
import Play.current
import scala.concurrent.Future


class ESCCalculatorControllerSpec extends UnitSpec with FakeCCCalculatorApplication with MockitoSugar with WithFakeApplication {

  val mockESCCalculatorController = new ESCCalculatorController with ESCCalculator {
    override val calculator = mock[ESCCalculatorService]
    override val auditEvent = mock[AuditEvents]
  }

  implicit val request = FakeRequest()

  "ESCCalculatorController" should {

    "not return NOT_FOUND (calculate) endpoint" in {
      val result = route(FakeRequest(POST, "/cc-calculator/employer-supported-childcare/calculate"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "Accept valid JSON at /employer-supported-childcare/calculate" in {
      val controller = mockESCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/calculator_input_test.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.OK
    }

    "Accept valid JSON at /employer-supported-childcare/calculate (all fields with default values missing)" in {
      val controller = mockESCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/without_default_values.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.OK
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 Tax Year)" in {
      val controller = mockESCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/no_tax_year.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears",
          |         "validationErrors":[
          |            {
          |               "message":"Please provide at least 1 Tax Year",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative value in eligible months)" in {
      val controller = mockESCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_eligible_months.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
          |         "validationErrors":[
          |            {
          |               "message":"Number of months should not be less than 0 and not more than 99",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (value more than 99 in eligible months)" in {
      val controller = mockESCCalculatorController
      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/more_than_upper_limit_eligible_months.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)


      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
          |         "validationErrors":[
          |            {
          |               "message":"Number of months should not be less than 0 and not more than 99",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 periods)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/no_periods.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods",
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

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 claimants)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/no_claimants.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants",
          |         "validationErrors":[
          |            {
          |               "message":"Please provide at least 1 claimant",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative taxable pay)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_taxable_pay.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/income/taxablePay",
          |         "validationErrors":[
          |            {
          |               "message":"Taxable pay should not be less than 0.00",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative gross amount)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_gross.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST


      val outputJSON = Json.parse(
        """
         {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/income/gross",
          |         "validationErrors":[
          |            {
          |               "message":"Gross amount should not be less than 0.00",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative voucher amount)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_voucher_amount.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/escAmount",
          |         "validationErrors":[
          |            {
          |               "message":"Voucher amount should not be less than 0.00",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (date missing)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/date_missing.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/until",
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

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (incorrect data type)" in {
      val controller = mockESCCalculatorController
      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/incorrect_data_type.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[Request]())).thenReturn(Future.successful(AwardPeriod()))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/payload/eligibility/esc/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
          |         "validationErrors":[
          |            {
          |               "message":"error.expected.jsnumber",
          |               "args":[
          |
          |               ]
          |            }
          |         ]
          |      }
          |   ]
          |}
        """.stripMargin)

      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Return Internal Server Error with error message if an exception is thrown during calculation " in {
      val controller = mockESCCalculatorController
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/calculator_input_test.json")
      val inputJson: JsValue = Json.parse(resource.toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[Request]

      when(controller.calculator.award(mockEq(JsonResult.get))).thenReturn(Future.failed(new Exception("Something bad happened")))
      val result = await(controller.calculate()(request))
      val outputJSON = Json.parse(
        """
          |{
          |    "status": 500,
          |    "error": "Something bad happened"
          |}
        """.stripMargin)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

    "Return Bad Request with error message if a request for a different scheme is passed(e.g. TC) " in {
      val controller = mockESCCalculatorController
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_12.json")
      val inputJson: JsValue = Json.parse(resource.toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val result = await(controller.calculate()(request))

      val outputJSON = Json.parse(
        """
          |{
          |    "status": 400,
          |    "error": "You have provided a wrong type of request"
          |}
        """.stripMargin)

      status(result) shouldBe Status.BAD_REQUEST
      implicit val materializer = Play.application.materializer
      jsonBodyOf(result) shouldBe outputJSON
    }

  }

}
