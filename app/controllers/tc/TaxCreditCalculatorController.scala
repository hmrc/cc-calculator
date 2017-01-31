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

/**
 * Created by user on 03/06/15.
 */

import calculators.TCCalculator
import controllers.CalculatorController
import models.input.APIModels.Request
import play.api.Logger
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import play.api.i18n.Messages
import play.api.libs.json._
import play.api.mvc.Action
import service.AuditEvents
import uk.gov.hmrc.play.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TaxCreditCalculatorController extends TaxCreditCalculatorController with TCCalculator {
  override val auditEvent = AuditEvents
}

trait TaxCreditCalculatorController extends CalculatorController with ServicesConfig {
  this: TCCalculator =>

  val auditEvent : AuditEvents

  def incomeAdvice: Action[JsValue] = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn("TC Calculator Validation JsError in TaxCreditCalculatorController.incomeAdvice")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          result.getTaxCreditsEligibility.isSuccess match {
            case true =>
              calculator.incomeAdvice(result).map {
                response =>
                  Ok(utils.JSONFactory.generateResultJson(response))
              } recover {
                case e: Exception =>
                  Logger.warn(s"Tax Credits Calculator Exception in TaxCreditCalculatorController.incomeAdvice: ${e.getMessage}")
                  InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
              }
            case _ =>
              Logger.warn("Tax Credits Calculator Exception in TaxCreditCalculatorController.incomeAdvice")
              Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(
                play.api.http.Status.BAD_REQUEST,
                Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception")))
              )))
          }
        }
      )
  }

 override def calculate: Action[JsValue] = Action.async(parse.json) {
   implicit request =>
     request.body.validate[Request].fold(
       error => {
         Logger.warn(s"\n\n TC Calculator Validation JsError in TaxCreditCalculatorController.calculate \n\n")
         Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
       },
       result => {
         result.getTaxCreditsEligibility.isSuccess match {
           case true =>
             auditEvent.auditTCRequest(result.toString)
             calculator.award(result).map {
               response =>
                 auditEvent.auditTCResponse(utils.JSONFactory.generateResultJson(response).toString())
                 Ok(utils.JSONFactory.generateResultJson(response))
             } recover {
               case e: Exception =>
                 Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate: ${e.getMessage}\n\n")
                 InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
             }
           case _ =>
             Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate \n\n")
             Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(
               play.api.http.Status.BAD_REQUEST,
               Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception")))
             )))
         }
       }
     )
 }

}
