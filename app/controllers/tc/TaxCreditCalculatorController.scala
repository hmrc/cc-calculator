/*
 * Copyright 2022 HM Revenue & Customs
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
import javax.inject.{Inject, Singleton}
import models.input.tc.TCCalculatorInput
import play.api.Logging
import play.api.i18n.I18nSupport
import play.api.libs.json._
import play.api.mvc.{Action, MessagesControllerComponents}
import service.AuditEvents
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JSONFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class TaxCreditCalculatorController @Inject()(val mcc: MessagesControllerComponents,
                                              val auditEvent: AuditEvents,
                                              val calculator: TCCalculator) extends BackendController(mcc)
  with I18nSupport with Logging  {

  def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[TCCalculatorInput].fold(
        error => {
          logger.warn(s"TC Calculator Validation JsError in TaxCreditCalculatorController.calculate")
          Future.successful(BadRequest(JSONFactory.generateErrorJSON(BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditTCRequest(result.toString)
          calculator.award(result).map {
            response =>
              auditEvent.auditTCResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              logger.warn(s"Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate: ${e.getMessage}")
              InternalServerError(JSONFactory.generateErrorJSON(INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }

}
