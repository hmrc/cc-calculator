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

package models.input.tc

import models.input.APIModels._
import org.joda.time.LocalDate
import play.api.data.validation._
import play.api.i18n.Messages
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils._


case class TCEligibility(
                          taxYears: List[TaxYear],
                          proRataEnd: Option[LocalDate] = None
                          ) {

  def isProrateringRequired: Boolean = {
    proRataEnd.isDefined
  }
}
object TCEligibility {
  implicit val tcEligibilityFormat: Reads[TCEligibility] = (
      (JsPath \ "taxYears").read[List[TaxYear]] and
        ((JsPath \ "proRataEnd").readNullable[LocalDate](jodaLocalDateReads(datePattern)) or Reads.optionWithNull(jodaLocalDateReads(datePattern)))
    )(TCEligibility.apply _)
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    houseHoldIncome : BigDecimal,
                    periods: List[Period]
                    )

object TaxYear {
  def houseHoldIncomeValidation(income: BigDecimal) : Boolean = {
    income >= BigDecimal(0.00)
  }

  implicit val taxYearsFormat: Reads[TaxYear] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "houseHoldIncome").read[BigDecimal].filter(
        ValidationError(Messages("cc.calc.household.income.too.low"))
        )(x => houseHoldIncomeValidation(x)) and
          (JsPath \ "periods").read[List[Period]]
    )(TaxYear.apply _)
}

case class Period(from: LocalDate,
                  until: LocalDate,
                  householdElements: HouseHoldElements,
                  claimants: List[Claimant],
                  children: List[Child]
                   ) {

  def getChildCareForPeriod: Boolean = {
    householdElements.childcare
  }
  def config: TCTaxYearConfig = TCConfig.getConfig(from)

  def atLeastOneClaimantIsClaimingSocialSecurityBenefit: Boolean = {
    val count = claimants.foldLeft(0)((acc, claimant) => if (claimant.isClaimingSocialSecurity) acc + 1 else acc)
    count > 0
  }
}

object Period {
  implicit val periodFormat: Reads[Period] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "householdElements").read[HouseHoldElements] and
          (JsPath \ "claimants").read[List[Claimant]] and
            (JsPath \ "children").read[List[Child]]
    )(Period.apply _)
}

case class ChildElements(child: Boolean = false,
                         youngAdult: Boolean = false,
                         disability: Boolean = false,
                         severeDisability: Boolean = false,
                         childcare: Boolean = false)

object ChildElements {
  implicit val childElementsReads: Reads[ChildElements] = (
    (JsPath \ "child").read[Boolean] and
      (JsPath \ "youngAdult").read[Boolean] and
        (JsPath \ "disability").read[Boolean] and
          (JsPath \ "severeDisability").read[Boolean] and
            (JsPath \ "childcare").read[Boolean]
    )(ChildElements.apply _)
}

case class Child(id: Short,
                 name : String,
                 qualifying: Boolean = false,
                 childcareCost : BigDecimal,
                 childcareCostPeriod: Periods.Period,
                 childElements: ChildElements) {

  def isQualifyingWTC: Boolean = {
    qualifying && childElements.childcare
  }

  def isQualifyingCTC: Boolean = {
    qualifying && (childElements.child || childElements.youngAdult)
  }

  def getsDisabilityElement: Boolean = {
    isQualifyingCTC && childElements.disability
  }

  def getsSevereDisabilityElement: Boolean = {
    isQualifyingCTC && childElements.severeDisability
  }

}

object Child  {
  def validID(id: Short): Boolean = {
    id >= 0
  }

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childFormat: Reads[Child] = (
    (JsPath \ "id").read[Short].filter(ValidationError(Messages("cc.calc.id.should.not.be.less.than.0")))(x => validID(x)) and
      (JsPath \ "name").read[String](maxLength[String](TCConfig.maxNameLength)) and
       (JsPath \ "qualifying").read[Boolean] and
        (JsPath \ "childcareCost").read[BigDecimal].filter(
        ValidationError(Messages("cc.calc.childcare.spend.too.low"))
        )(x => childSpendValidation(x)) and
        //childcareCost max value should be 30,000 per year (This will be based on childcareCost Period, hence should be handled in frontend)
          (JsPath \ "childcareCostPeriod").read[Periods.Period] and
            (JsPath \ "childElements").read[ChildElements]
    )(Child.apply _)
}

case class ClaimantDisability(
                             disability: Boolean = false,
                             severeDisability: Boolean = false
                             )

object ClaimantDisability {
  implicit val claimantDisabilityFormat: Reads[ClaimantDisability] = (
    (JsPath \ "disability").read[Boolean] and
      (JsPath \ "severeDisability").read[Boolean]
    )(ClaimantDisability.apply _)
}

case class Claimant(qualifying: Boolean,
                    isPartner: Boolean,
                    claimantElements: ClaimantDisability,
                    doesNotTaper : Boolean = false,
                    failures: Option[List[String]]) {
  def isQualified: Boolean = {
    qualifying
  }

  def getsDisabilityElement: Boolean = {
    isQualified && claimantElements.disability
  }

  def getsSevereDisabilityElement: Boolean = {
    isQualified && claimantElements.severeDisability
  }

  def isClaimingSocialSecurity: Boolean = {
    doesNotTaper
  }
}

object Claimant {

  implicit val claimantFormat: Reads[Claimant] = (
    (JsPath \ "qualifying").read[Boolean] and
      (JsPath \ "isPartner").read[Boolean] and
        (JsPath \ "claimantElements").read[ClaimantDisability] and
          ((JsPath \ "doesNotTaper").read[Boolean] or Reads.pure(false)) and
            (JsPath \ "failures").readNullable[List[String]]
    )(Claimant.apply _)
}

case class HouseHoldElements(basic: Boolean = false,
                             hours30: Boolean = false,
                             childcare: Boolean = false,
                             loneParent: Boolean = false,
                             secondParent: Boolean = false,
                             family: Boolean = false
                              )

object HouseHoldElements {
  implicit val householdElementsFormat: Reads[HouseHoldElements] = (
    (JsPath \ "basic").read[Boolean] and
      (JsPath \ "hours30").read[Boolean] and
        (JsPath \ "childcare").read[Boolean] and
          (JsPath \ "loneParent").read[Boolean] and
            (JsPath \ "secondParent").read[Boolean] and
              (JsPath \ "family").read[Boolean]
    )(HouseHoldElements.apply _)
}
