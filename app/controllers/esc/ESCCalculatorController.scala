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
import controllers.CalculatorController
import models.input.APIModels.Request
import models.input.esc.ESCEligibility
import models.output.esc.ESCCalculation
import play.api.Logger
import play.api.i18n.Messages
import play.api.libs.json.{Json, JsValue, JsError}
import play.api.mvc.Action
import service.AuditEvents
import play.api.i18n.{I18nSupport, MessagesApi}
import javax.inject.{Inject, Singleton}

import uk.gov.hmrc.play.microservice.controller.BaseController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class ESCCalculatorController @Inject()(val messagesApi: MessagesApi) extends BaseController with ESCCalculator with I18nSupport {

  this: ESCCalculator =>

  val auditEvent : AuditEvents = AuditEvents

  def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[ESCEligibility].fold(
        error => {
          Logger.warn("ESC Calculator Validation JsError in ESCCalculatorController.calculate")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditESCRequest(result.toString)
          calculator.award(result).map {
            response =>
              val jsonResponse = Json.toJson[ESCCalculation](response)
              auditEvent.auditESCResponse(jsonResponse.toString())
              Ok(jsonResponse)
          } recover {
            case e: Exception =>
              Logger.warn(s"ESC Calculator Exception in ESCCalculatorController.calculate: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }
}
