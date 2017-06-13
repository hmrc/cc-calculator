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

package controllers.tc

import javax.inject.{Inject, Singleton}

import calculators.TCCalculator
import models.input.tc.TCEligibility
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json._
import play.api.mvc.Action
import service.AuditEvents
import uk.gov.hmrc.play.microservice.controller.BaseController
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@Singleton
class TaxCreditCalculatorController @Inject()(val messagesApi: MessagesApi) extends BaseController with
  TCCalculator with I18nSupport {


  this: TCCalculator =>

  val auditEvent: AuditEvents = AuditEvents

  def incomeAdvice: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[TCEligibility].fold(
        error => {
          Logger.warn("TC Calculator Validation JsError in TaxCreditCalculatorController.incomeAdvice")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          calculator.incomeAdvice(result).map {
            response =>
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"Tax Credits Calculator Exception in TaxCreditCalculatorController.incomeAdvice: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }

        }
      )
  }

  def calculate: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[TCEligibility].fold(
        error => {
          Logger.warn(s"TC Calculator Validation JsError in TaxCreditCalculatorController.calculate")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          println("--------------------" + result)
          auditEvent.auditTCRequest(result.toString)
          calculator.award(result).map {
            response =>
              auditEvent.auditTCResponse(Json.toJson(response).toString())
              Ok(Json.toJson(response))
          } recover {
            case e: Exception =>
              Logger.warn(s"Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate: ${e.getMessage}")
              InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
          }
        }
      )
  }

}
