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

package controllers

import calculators._
import com.github.fge.jackson.JsonLoader
import models.input.esc.ESCCalculatorInput
import models.input.tc.TCCalculatorInput
import models.input.tfc.TFCCalculatorInput
import models.output.CalculatorOutput
import models.output.esc.{ESCCalculatorOutput, ESCSavings}
import models.output.tc.TCCalculatorOutput
import models.output.tfc.{TFCCalculatorOutput, TFCContribution}
import java.time.LocalDate
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import service.AuditEvents
import utils.{FakeCCCalculatorApplication, TFCConfig}
import org.mockito.ArgumentMatchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.Future

class CalculatorControllerSpec extends FakeCCCalculatorApplication with MockitoSugar with BeforeAndAfterEach {
  implicit val request = FakeRequest("POST", "").withHeaders("Content-Type" -> "application/json")

  lazy val audits = app.injector.instanceOf[AuditEvents]
  lazy val tcc = app.injector.instanceOf[TCCalculator]
  lazy val tfc = app.injector.instanceOf[TFCCalculator]
  lazy val esc = app.injector.instanceOf[ESCCalculator]
  lazy val tcfConfig = app.injector.instanceOf[TFCConfig]

  "CalculatorController" must {

    "be available" in {
      val result = route(app, FakeRequest(POST, "/cc-calculator/calculate"))
      result.isDefined shouldBe true
      status(result.get) should not be NOT_FOUND
    }

    "calling calculate" must {

      "return BAD_REQUEST" when {
        "invalid request is given" in {
          val sut = new CalculatorController(mcc, audits, tcc, tfc, esc, tcfConfig)

          val result = await(sut.calculate()(request.withBody(Json.obj("tc" -> "test", "tfc" -> "test", "esc" -> "test"))))
          status(result) shouldBe BAD_REQUEST
        }
      }

      "calculate savings" when {

        val stubbedTCC = mock[TCCalculator]
        val stubbedTFC = mock[TFCCalculator]
        val stubbedESC = mock[ESCCalculator]

        "valid data is given" when {

          "request doesn't contain any data" in {
            val sut = new CalculatorController(mcc, audits, tcc, tfc, esc, tcfConfig)

            val result = await(sut.calculate()(request.withBody(Json.obj())))
            status(result) shouldBe OK
            jsonBodyOf(result) shouldBe Json.toJson(CalculatorOutput(tcAmount = None, tfcAmount = None, escAmount = None))
          }

          "request contains data only for TC" in {
            val sut = new CalculatorController(mcc, audits, stubbedTCC, tfc, esc, tcfConfig)
            val validInput: JsValue = Json.parse(JsonLoader.fromResource("/json/tc/input/valid_json.json").toString)

            when(stubbedTCC.award(any[TCCalculatorInput]))
              .thenReturn(Future.successful(
                TCCalculatorOutput(
                  from = LocalDate.now,
                  until = LocalDate.now.plusYears(1),
                  houseHoldAdviceAmount = BigDecimal(0.00),
                  totalAwardAmount = BigDecimal(100.00),
                  taxYears = List.empty
                )
              )
            )

            val result = await(sut.calculate()(request.withBody(Json.obj("tc" -> validInput))))
            status(result) shouldBe OK
            jsonBodyOf(result) shouldBe Json.toJson(CalculatorOutput(tcAmount = Some(100), tfcAmount = None, escAmount = None))
          }

          "request contains data only for TFC" in {
            val sut = new CalculatorController(mcc, audits, tcc, stubbedTFC, esc, tcfConfig)

            val validInput: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/calculator_input_test.json").toString)

            when(stubbedTFC.award(any[TFCCalculatorInput]))
              .thenReturn(Future.successful(
                TFCCalculatorOutput(
                  householdContribution = TFCContribution(parent = 0, government = 200, totalChildCareSpend = 0),
                  numberOfPeriods = 0,
                  periods = List.empty
                )
              )
            )

            val result = await(sut.calculate()(request.withBody(Json.obj("tfc" -> validInput))))
            status(result) shouldBe OK
            jsonBodyOf(result) shouldBe Json.toJson(CalculatorOutput(tcAmount = None, tfcAmount = Some(200), escAmount = None))
          }

          "request contains data only for ESC" in {
            val sut = new CalculatorController(mcc, audits, tcc, tfc, stubbedESC, tcfConfig)

            val validInput: JsValue = Json.parse(JsonLoader.fromResource(s"/json/esc/input/calculator_input_test.json").toString)

            when(stubbedESC.award(any[ESCCalculatorInput]))
              .thenReturn(Future.successful(
                ESCCalculatorOutput(
                  from = LocalDate.now,
                  until = LocalDate.now.plusYears(1),
                  totalSavings = ESCSavings(
                    taxSaving = 0,
                    niSaving = 0,
                    totalSaving = 300
                  ),
                  taxYears = List.empty
                )
              )
            )

            val result = await(sut.calculate()(request.withBody(Json.obj("esc" -> validInput))))
            status(result) shouldBe OK
            jsonBodyOf(result) shouldBe Json.toJson(CalculatorOutput(tcAmount = None, tfcAmount = None, escAmount = Some(300)))
          }

          "request contains data for TC, TFC and ESC" in {
            val sut = new CalculatorController(mcc, audits, stubbedTCC, stubbedTFC, stubbedESC, tcfConfig)

            val validTCInput: JsValue = Json.parse(JsonLoader.fromResource("/json/tc/input/valid_json.json").toString)
            val validTFCInput: JsValue = Json.parse(JsonLoader.fromResource("/json/tfc/input/calculator_input_test.json").toString)
            val validESCInput: JsValue = Json.parse(JsonLoader.fromResource(s"/json/esc/input/calculator_input_test.json").toString)

            when(stubbedTCC.award(any[TCCalculatorInput]))
              .thenReturn(Future.successful(
                TCCalculatorOutput(
                  from = LocalDate.now,
                  until = LocalDate.now.plusYears(1),
                  houseHoldAdviceAmount = BigDecimal(0.00),
                  totalAwardAmount = BigDecimal(100.00),
                  taxYears = List.empty
                )
              )
            )

            when(stubbedTFC.award(any[TFCCalculatorInput]))
              .thenReturn(Future.successful(
                TFCCalculatorOutput(
                  householdContribution = TFCContribution(parent = 0, government = 200, totalChildCareSpend = 0),
                  numberOfPeriods = 0,
                  periods = List.empty
                )
              )
            )

            when(stubbedESC.award(any[ESCCalculatorInput]))
              .thenReturn(Future.successful(
                ESCCalculatorOutput(
                  from = LocalDate.now,
                  until = LocalDate.now.plusYears(1),
                  totalSavings = ESCSavings(
                    taxSaving = 0,
                    niSaving = 0,
                    totalSaving = 300
                  ),
                  taxYears = List.empty
                )
              )
            )

            val result = await(sut.calculate()(request.withBody(Json.obj("tc" -> validTCInput, "tfc" -> validTFCInput, "esc" -> validESCInput))))
            status(result) shouldBe OK
            jsonBodyOf(result) shouldBe Json.toJson(CalculatorOutput(tcAmount = Some(100), tfcAmount = Some(200), escAmount = Some(300)))
          }

        }
      }
    }

  }
}
