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

package controllers.tfc

import calculators.TFCCalculator
import controllers.CalculatorController
import models.input.APIModels.Request
import play.api.Logger
import play.api.i18n.Messages
import play.api.libs.json.JsValue
import play.api.mvc.Action
import service.AuditEvents
import play.api.i18n.{I18nSupport, MessagesApi}
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class TFCCalculatorController @Inject()(val messagesApi: MessagesApi) extends
  CalculatorController with TFCCalculator with I18nSupport {

  this : TFCCalculator =>

  val auditEvent : AuditEvents = AuditEvents

  override def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"TFC Calculator Validation JsError in TFCCalculatorController.calculate")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          result.getTFCEligibility.isSuccess match {
            case true =>
              auditEvent.auditTFCRequest(result.toString)
              calculator.award(result).map {
                response =>
                  auditEvent.auditTFCResponse(utils.JSONFactory.generateResultJson(response).toString())
                  Ok(utils.JSONFactory.generateResultJson(response))
              } recover {
                case e: Exception =>
                  Logger.warn(s"TFC Calculator Exception in TFCCalculatorController.calculate: ${e.getMessage}")
                  InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
              }
            case _ =>
              Logger.warn(s"TFC Calculator Exception in TFCCalculatorController.calculate")
              Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(
                play.api.http.Status.BAD_REQUEST,
                Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception")))
              )))
          }

        }
      )
  }
}
