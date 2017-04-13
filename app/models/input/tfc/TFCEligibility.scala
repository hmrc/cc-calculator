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

package models.input.tfc

import org.joda.time.LocalDate
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils._

/**
 * Created by user on 18/06/15.
 */
case class TFCEligibility(
                           from: LocalDate,
                           until: LocalDate,
                           householdEligibility: Boolean,
                           periods: List[TFCPeriod]
                           )


object TFCEligibility extends CCFormat with MessagesObject {
  implicit val tfcEligibilityFormat: Reads[TFCEligibility] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "householdEligibility").read[Boolean] and
          (JsPath \ "periods").read[List[TFCPeriod]].filter(ValidationError(messages("cc.calc.invalid.number.of.periods")))(periods => periods.length > 0)
    )(TFCEligibility.apply _)
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodEligibility: Boolean,
                      children: List[Child]
                      ){
  def configRule : TFCTaxYearConfig = TFCConfig.getConfig(from)
}

object TFCPeriod extends CCFormat with MessagesObject {

  implicit val periodFormat : Reads[TFCPeriod] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "periodEligibility").read[Boolean] and
          (JsPath \ "children").read[List[Child]].filter(
          ValidationError(messages("cc.calc.invalid.number.of.children"))
          )(children => children.length > 0 && children.length <= TFCConfig.maxNoOfChildren)
    )(TFCPeriod.apply _)
}

case class Child(
                  id: Short,
                  name: Option[String],
                  qualifying: Boolean,
                  from: Option[LocalDate],
                  until: Option[LocalDate],
                  childcareCost : BigDecimal,
                  disability :Disability
                  ) {
  def getChildDisability: Boolean = {
    disability.disabled || disability.severelyDisabled
  }

  def getChildSevereDisability: Boolean = {
    disability.severelyDisabled
  }

}

object Child extends CCFormat with MessagesObject {
  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }
  implicit val childFormat : Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(messages("cc.calc.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "name").readNullable[String](maxLength[String](TFCConfig.maxNameLength)) and
        (JsPath \ "qualifying").read[Boolean] and
          ((JsPath \ "from").readNullable[LocalDate](jodaLocalDateReads(datePattern)) or Reads.optionWithNull(jodaLocalDateReads(datePattern))) and
            ((JsPath \ "until").readNullable[LocalDate](jodaLocalDateReads(datePattern)) or Reads.optionWithNull(jodaLocalDateReads(datePattern))) and
              (JsPath \ "childcareCost").read[BigDecimal].filter(ValidationError(messages("cc.calc.childcare.spend.too.low")))(x => childSpendValidation(x)) and
                (JsPath \ "disability").read[Disability]
    )(Child.apply _)
}

case class Disability(
                       disabled: Boolean = false,
                       severelyDisabled: Boolean = false
                       )

object Disability {
  implicit val disabilityReads: Reads[Disability] = (
    (JsPath \ "disabled").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "severelyDisabled").read[Boolean].orElse(Reads.pure(false))
    )(Disability.apply _)
}
