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

package models.input.tc

import org.joda.time.LocalDate
import play.api.data.validation._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import utils._

case class TCEligibility(
                          taxYears: List[TaxYear]
                          )

object TCEligibility {
  implicit val tcEligibilityFormat: Reads[TCEligibility] =
    (JsPath \ "taxYears").read[List[TaxYear]].map {taxYears => TCEligibility(taxYears)}
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    previousHouseholdIncome: Income,
                    currentHouseholdIncome: Income,
                    periods: List[Period]
                    )

object TaxYear extends MessagesObject {
  implicit val taxYearsFormat: Reads[TaxYear] = Json.reads[TaxYear]
}

case class Income(
                   employment: Option[List[BigDecimal]],
                   pension: Option[List[BigDecimal]],
                   other: Option[List[BigDecimal]],
                   benefits: Option[List[BigDecimal]],
                   statutory: Option[List[StatutoryIncome]]
                   )

object Income {
  implicit val incomeFormat: Reads[Income] = Json.reads[Income]
}

case class StatutoryIncome(
                            weeks: Double,
                            amount: BigDecimal
                            )
object StatutoryIncome {
  implicit val statutoryIncomeFormat: Reads[StatutoryIncome] = Json.reads[StatutoryIncome]
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
    val count = claimants.foldLeft(0)((acc, claimant) => if (claimant.doesNotTaper) acc + 1 else acc)
    count > 0
  }
}

object Period {
  implicit val periodFormat: Reads[Period] = Json.reads[Period]
}

case class ChildElements(child: Boolean = false,
                         youngAdult: Boolean = false,
                         disability: Boolean = false,
                         severeDisability: Boolean = false,
                         childcare: Boolean = false)

object ChildElements {
  implicit val childElementsReads: Reads[ChildElements] = Json.reads[ChildElements]
}

case class Child(qualifying: Boolean = false,
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

object Child extends MessagesObject {

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childFormat: Reads[Child] = (
       (JsPath \ "qualifying").read[Boolean] and
        (JsPath \ "childcareCost").read[BigDecimal].filter(
          ValidationError(messages("cc.calc.childcare.spend.too.low"))
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
  implicit val claimantDisabilityFormat: Reads[ClaimantDisability] = Json.reads[ClaimantDisability]
}

case class Claimant(qualifying: Boolean,
                    isPartner: Boolean,
                    claimantElements: ClaimantDisability,
                    doesNotTaper : Boolean = false) {

  def getsDisabilityElement: Boolean = {
    qualifying && claimantElements.disability
  }

  def getsSevereDisabilityElement: Boolean = {
    qualifying && claimantElements.severeDisability
  }

}

object Claimant {

  implicit val claimantFormat: Reads[Claimant] = (
    (JsPath \ "qualifying").read[Boolean] and
      (JsPath \ "isPartner").read[Boolean] and
        (JsPath \ "claimantElements").read[ClaimantDisability] and
          ((JsPath \ "doesNotTaper").read[Boolean] or Reads.pure(false))
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
  implicit val householdElementsFormat: Reads[HouseHoldElements] = Json.reads[HouseHoldElements]
}
