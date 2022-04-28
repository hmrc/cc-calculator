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

package controllers.tfc

import calculators.TFCCalculator
import javax.inject.{Inject, Singleton}
import models.input.tfc.TFCCalculatorInput
import models.output.tfc.TFCCalculatorOutput
import play.api.Logging
import play.api.i18n.I18nSupport
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, MessagesControllerComponents}
import service.AuditEvents
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class TFCCalculatorController @Inject()(val mcc: MessagesControllerComponents,
                                        val auditEvent: AuditEvents,
                                        val calculator: TFCCalculator) extends BackendController(mcc)
  with I18nSupport with Logging {

  def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[TFCCalculatorInput].fold(
        error => {
          logger.warn(s"TFC Calculator Validation JsError in TFCCalculatorController.calculate>>>$error")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          auditEvent.auditTFCRequest(result.toString)
          calculator.award(result).map {
            response =>
              val jsonResponse = Json.toJson[TFCCalculatorOutput](response)
              auditEvent.auditTFCResponse(jsonResponse.toString())
              Ok(jsonResponse)
          } recover {
            case e: Exception =>
              logger.warn(s"TFC Calculator Exception in TFCCalculatorController.calculate: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }

      )
  }
}
