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

package controllers.esc

import calculators.ESCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.esc.ESCEligibility
import models.output.esc.{Savings, ESCCalculation}
import org.mockito.Matchers.{eq => mockEq, _}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mock.MockitoSugar
import play.api.http.Status
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents
import play.api.Play
import utils.FakeCCCalculatorApplication
import scala.concurrent.Future
import play.api.i18n.Messages.Implicits._
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.joda.time.LocalDate

class ESCCalculatorControllerSpec extends FakeCCCalculatorApplication with MockitoSugar with BeforeAndAfterEach {

  "ESCCalculatorController" should {

    "not return NOT_FOUND (calculate) endpoint" in {
      val result = route(app, FakeRequest(POST, "/cc-calculator/employer-supported-childcare/calculate"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    val validData = Table(
      ("Description", "Data"),
      ("valid data", "calculator_input_test.json"),
      ("all fields with default values missing", "without_default_values.json")
    )

    forAll(validData) { case (description, data) =>
      s"Accept valid JSON at /employer-supported-childcare/calculate (${description})" in {
        val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
          override val calculator = mock[ESCCalculatorService]
          override val auditEvent = mock[AuditEvents]
        }

        val inputJson: JsValue = Json.parse(JsonLoader.fromResource(s"/json/esc/input/${data}").toString)
        val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

        when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(ESCCalculation(from = LocalDate.now, until = LocalDate.now, totalSavings = Savings(), taxYears = List())))
        val result = await(controller.calculate()(request))
        status(result) shouldBe Status.OK
      }
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 Tax Year)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/no_tax_year.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative value in eligible months)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_eligible_months.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (value more than 99 in eligible months)" in {

      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson: JsValue = Json.parse(JsonLoader.fromResource("/json/esc/input/more_than_upper_limit_eligible_months.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)


      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 periods)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/no_periods.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods",
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

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (0 claimants)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/no_claimants.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative taxable pay)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_taxable_pay.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/income/taxablePay",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative gross amount)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_gross.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST


      val outputJSON = Json.parse(
        """
         {
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/income/gross",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (negative voucher amount)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/negative_voucher_amount.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/escAmount",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (date missing)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/date_missing.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/until",
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

    "Accept invalid JSON at /employer-supported-childcare/calculate and return a BadRequest with an error (incorrect data type)" in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val inputJson = Json.parse(JsonLoader.fromResource("/json/esc/input/incorrect_data_type.json").toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)

      when(controller.calculator.award(any[ESCEligibility]())).thenReturn(Future.successful(mock[ESCCalculation]))
      val result = await(controller.calculate()(request))
      status(result) shouldBe Status.BAD_REQUEST

      val outputJSON = Json.parse(
        """
          |{
          |   "status":400,
          |   "errors":[
          |      {
          |         "path":"/taxYears(0)/periods(0)/claimants(0)/eligibleMonthsInPeriod",
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

      jsonBodyOf(result) shouldBe outputJSON
    }

    "Return Internal Server Error with error message if an exception is thrown during calculation " in {
      val controller = new ESCCalculatorController(applicationMessagesApi) with ESCCalculator {
        override val calculator = mock[ESCCalculatorService]
        override val auditEvent = mock[AuditEvents]
      }

      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/calculator_input_test.json")
      val inputJson: JsValue = Json.parse(resource.toString)
      val request: FakeRequest[JsValue] = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json").withBody(inputJson)
      val JsonResult = inputJson.validate[ESCEligibility]

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
      jsonBodyOf(result) shouldBe outputJSON
    }

  }

}
