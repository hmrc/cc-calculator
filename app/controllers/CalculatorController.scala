/*
 * Copyright 2020 HM Revenue & Customs
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

import calculators.{ESCCalculator, TCCalculator, TFCCalculator}
import javax.inject.{Inject, Singleton}
import models.input.CalculatorInput
import models.input.esc.ESCCalculatorInput
import models.input.tc.TCCalculatorInput
import models.input.tfc.TFCCalculatorInput
import models.output.CalculatorOutput
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, MessagesControllerComponents}
import service.AuditEvents
import uk.gov.hmrc.play.bootstrap.controller.BackendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class CalculatorController @Inject()(val mcc: MessagesControllerComponents,
                                     val auditEvent: AuditEvents,
                                     val tcCalculator: TCCalculator,
                                     val tfcCalculator: TFCCalculator,
                                     val escCalculator: ESCCalculator) extends BackendController(mcc) with I18nSupport {

  def calculate: Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body.validate[CalculatorInput].fold(
      error => {
        Logger.warn("Calculator Validation JsError in CalculatorController.calculate")
        Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
      },
      result => {
        auditEvent.auditRequest(result.toString)

        for {
          tcRes <- calculateTC(result.tc)
          tfcRes <- calculateTFC(result.tfc)
          escRes <- calculateESC(result.esc)
        } yield Ok(Json.toJson[CalculatorOutput](CalculatorOutput(tcAmount = tcRes, tfcAmount = tfcRes, escAmount = escRes)))

      }
    )
  }

  private def calculateTC(tc: Option[TCCalculatorInput]): Future[Option[BigDecimal]] = {
    if(tc.isDefined) {
      tcCalculator.award(tc.get).map { result =>
        Some(result.totalAwardAmount)
      }
    }
    else {
      Future.successful(None)
    }
  }

  private def calculateTFC(tfc: Option[TFCCalculatorInput]): Future[Option[BigDecimal]] = {
    if(tfc.isDefined) {
      tfcCalculator.award(tfc.get).map { result =>
        Some(result.householdContribution.government)
      }
    }
    else {
      Future.successful(None)
    }
  }

  private def calculateESC(esc: Option[ESCCalculatorInput]): Future[Option[BigDecimal]] = {
    if(esc.isDefined) {
      escCalculator.award(esc.get).map { result =>
        Some(result.totalSavings.totalSaving)
      }
    }
    else {
      Future.successful(None)
    }
  }
}
