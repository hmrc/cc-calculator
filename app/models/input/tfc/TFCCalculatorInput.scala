/*
 * Copyright 2019 HM Revenue & Customs
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

package models.input.tfc

import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils._

case class TFCCalculatorInput(
                               from: LocalDate,
                               until: LocalDate,
                               householdEligibility: Boolean,
                               periods: List[TFCPeriod]
                             )

object TFCCalculatorInput extends MessagesObject {
  implicit val tfcEligibilityFormat: Reads[TFCCalculatorInput] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "householdEligibility").read[Boolean] and
          (JsPath \ "periods").read[List[TFCPeriod]].filter(ValidationError(messages("cc.calc.invalid.number.of.periods")))(periods => periods.nonEmpty)
    )(TFCCalculatorInput.apply _)
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodEligibility: Boolean,
                      children: List[TFCChild]
                    ){
  def configRule : TFCTaxYearConfig = TFCConfig.getConfig(from)
}

object TFCPeriod extends MessagesObject {

  implicit val periodFormat : Reads[TFCPeriod] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "periodEligibility").read[Boolean] and
          (JsPath \ "children").read[List[TFCChild]].filter(ValidationError(messages("cc.calc.invalid.number.of.children"))
          )(children => children.nonEmpty && children.length <= TFCConfig.maxNoOfChildren)
    )(TFCPeriod.apply _)
}

case class TFCChild(
                     qualifying: Boolean,
                     from: Option[LocalDate],
                     until: Option[LocalDate],
                     childcareCost: BigDecimal,
                     childcareCostPeriod: Periods.Period = Periods.Monthly,
                     disability: TFCDisability
                   ) {
  def getChildDisability: Boolean = {
    disability.disabled || disability.severelyDisabled
  }
}

object TFCChild extends MessagesObject {

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }
  implicit val childFormat : Reads[TFCChild] = (
    (JsPath \ "qualifying").read[Boolean] and
      ((JsPath \ "from").readNullable[LocalDate](jodaLocalDateReads(datePattern)) or Reads.optionWithNull(jodaLocalDateReads(datePattern))) and
        ((JsPath \ "until").readNullable[LocalDate](jodaLocalDateReads(datePattern)) or Reads.optionWithNull(jodaLocalDateReads(datePattern))) and
          (JsPath \ "childcareCost").read[BigDecimal].filter(ValidationError(messages("cc.calc.childcare.spend.too.low")))(x => childSpendValidation(x)) and
            (JsPath \ "childcareCostPeriod").read[Periods.Period] and
              (JsPath \ "disability").read[TFCDisability]
    )(TFCChild.apply _)
}

case class TFCDisability(
                          disabled: Boolean = false,
                          severelyDisabled: Boolean = false
                        )

object TFCDisability {
  implicit val disabilityReads: Reads[TFCDisability] = Json.reads[TFCDisability]
}
