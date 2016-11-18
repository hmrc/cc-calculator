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

package controllers.tfc

import calculators.TFCCalculator
import controllers.CalculatorController
import models.input.APIModels.Request
import play.api.Logger
import play.api.i18n.Messages
import play.api.libs.json.{JsValue, JsError}
import play.api.mvc.Action
import service.AuditEvents
import uk.gov.hmrc.play.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TFCCalculatorController extends TFCCalculatorController with TFCCalculator {
  override val auditEvent = AuditEvents
}

trait TFCCalculatorController extends CalculatorController with ServicesConfig {

  this : TFCCalculator =>

  val auditEvent : AuditEvents

  override def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>

      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"\n\n TFC Calculator Validation JsError in TFCCalculatorController.calculate \n\n")
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
                  Logger.warn(s"\n\n TFC Calculator Exception in TFCCalculatorController.calculate: ${e.getMessage}\n\n")
                  InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
              }
            case _ =>
              Logger.warn(s"\n\n TFC Calculator Exception in TFCCalculatorController.calculate \n\n")
              Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(
                play.api.http.Status.BAD_REQUEST,
                Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception")))
              )))
          }

        }
      )
  }
}
