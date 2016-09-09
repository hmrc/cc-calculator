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

package controllers.tc

/**
 * Created by user on 03/06/15.
 */

import calculators.TCCalculator
import controllers.CalculatorController
import models.input.APIModels.Request
import play.api.Logger
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

  def incomeAdvice = Action.async(parse.json) {
    implicit request =>
      request.body.validate[Request].fold(
        error => {
          Logger.warn(s"\n\n TC Calculator Validation JsError in TaxCreditCalculatorController.incomeAdvice: ${JsError.toFlatJson(error).toString()}\n\n")
          Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
        },
        result => {
          result.getTaxCreditsEligibility.isSuccess match {
            case true =>
              Logger.debug(s"\n\n TC Calculator Validation passed in TaxCreditCalculatorController.incomeAdvice: ${result.toString}\n\n")
              calculator.incomeAdvice(result).map {
                response =>
                  Logger.debug(s"\n\n TC Calculator Result in TaxCreditCalculatorController.incomeAdvice: ${response.toString}\n\n")
                  Ok(utils.JSONFactory.generateResultJson(response))
              } recover {
                case e: Exception =>
                  Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.incomeAdvice: ${e.getMessage}\n\n")
                  InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
              }
            case _ =>
              Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.incomeAdvice: Input Request::: ${result.toString}\n\n")
              Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception"))))))
          }
        }
      )
  }

 override def calculate = Action.async(parse.json) {
   implicit request =>
     request.body.validate[Request].fold(
       error => {
         Logger.warn(s"\n\n TC Calculator Validation JsError in TaxCreditCalculatorController.calculate: ${JsError.toFlatJson(error).toString()}\n\n")
         Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Left(error))))
       },
       result => {
         result.getTaxCreditsEligibility.isSuccess match {
           case true =>
             auditEvent.auditTCRequest(result.toString)
             Logger.debug(s"\n\n TC Calculator Validation passed in TaxCreditCalculatorController.calculate: ${result.toString}\n\n")
             calculator.award(result).map {
               response =>
                 Logger.debug(s"\n\n TC Calculator Result in TaxCreditCalculatorController.calculate: ${response.toString}\n\n")
                 auditEvent.auditTCResponse(utils.JSONFactory.generateResultJson(response).toString())
                 Ok(utils.JSONFactory.generateResultJson(response))
             } recover {
               case e: Exception =>
                 Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate: ${e.getMessage}\n\n")
                 InternalServerError(utils.JSONFactory.generateErrorJSON(play.api.http.Status.INTERNAL_SERVER_ERROR, Right(e)))
             }
           case _ =>
             Logger.warn(s"\n\n Tax Credits Calculator Exception in TaxCreditCalculatorController.calculate: Input Request::: ${result.toString}\n\n")
             Future.successful(BadRequest(utils.JSONFactory.generateErrorJSON(play.api.http.Status.BAD_REQUEST, Right(new IllegalArgumentException(Messages("cc.calc.invalid.request.exception"))))))
         }
       }
     )
 }

}
