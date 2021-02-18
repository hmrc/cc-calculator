/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.JodaReads._
import play.api.libs.json.JodaWrites._
import utils._

case class TCCalculatorInput(
                          taxYears: List[TCTaxYear]
                          )

object TCCalculatorInput {
  implicit val tcEligibilityFormat: Reads[TCCalculatorInput] =
    (JsPath \ "taxYears").read[List[TCTaxYear]].map { taxYears => TCCalculatorInput(taxYears)}
}

case class TCTaxYear(
                      from: LocalDate,
                      until: LocalDate,
                      previousHouseholdIncome: TCIncome,
                      currentHouseholdIncome: TCIncome,
                      periods: List[TCPeriod]
                    )

object TCTaxYear extends MessagesObject {
  implicit val taxYearsFormat: Reads[TCTaxYear] = Json.reads[TCTaxYear]
}

case class TCIncome(
                   employment: Option[List[BigDecimal]],
                   pension: Option[List[BigDecimal]],
                   other: Option[List[BigDecimal]],
                   benefits: Option[List[BigDecimal]],
                   statutory: Option[List[TCStatutoryIncome]]
                   )

object TCIncome {
  implicit val incomeFormat: Reads[TCIncome] = Json.reads[TCIncome]
}

case class TCStatutoryIncome(
                            weeks: Double,
                            amount: BigDecimal
                            )
object TCStatutoryIncome {
  implicit val statutoryIncomeFormat: Reads[TCStatutoryIncome] = Json.reads[TCStatutoryIncome]
}

case class TCPeriod(from: LocalDate,
                    until: LocalDate,
                    householdElements: TCHouseholdElements,
                    claimants: List[TCClaimant],
                    children: List[TCChild]
                 ) {

  def getChildCareForPeriod: Boolean = {
    householdElements.childcare
  }

 def atLeastOneClaimantIsClaimingSocialSecurityBenefit: Boolean = {
    val count = claimants.foldLeft(0)((acc, claimant) => if (claimant.doesNotTaper) acc + 1 else acc)
    count > 0
  }
}

object TCPeriod {
  implicit val periodFormat: Reads[TCPeriod] = Json.reads[TCPeriod]
}

case class TCChildElements(child: Boolean = false,
                           youngAdult: Boolean = false,
                           disability: Boolean = false,
                           severeDisability: Boolean = false,
                           childcare: Boolean = false)

object TCChildElements {
  implicit val childElementsReads: Reads[TCChildElements] = Json.reads[TCChildElements]
}

case class TCChild(qualifying: Boolean = false,
                   childcareCost : BigDecimal,
                   childcareCostPeriod: Periods.Period = Periods.Monthly,
                   childElements: TCChildElements) {

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

object TCChild extends MessagesObject {

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childFormat: Reads[TCChild] = (
       (JsPath \ "qualifying").read[Boolean] and
        (JsPath \ "childcareCost").read[BigDecimal].filter(
          JsonValidationError("Childcare Spend cost should not be less than 0.00")
        )(x => childSpendValidation(x)) and
        //childcareCost max value should be 30,000 per year (This will be based on childcareCost Period, hence should be handled in frontend)
          (JsPath \ "childcareCostPeriod").read[Periods.Period] and
            (JsPath \ "childElements").read[TCChildElements]
    )(TCChild.apply _)
}

case class TCDisability(
                             disability: Boolean = false,
                             severeDisability: Boolean = false
                             )

object TCDisability {
  implicit val claimantDisabilityFormat: Reads[TCDisability] = Json.reads[TCDisability]
}

case class TCClaimant(qualifying: Boolean,
                      isPartner: Boolean,
                      claimantDisability: TCDisability,
                      doesNotTaper : Boolean = false) {

  def getsDisabilityElement: Boolean = {
    qualifying && claimantDisability.disability
  }

  def getsSevereDisabilityElement: Boolean = {
    qualifying && claimantDisability.severeDisability
  }

}

object TCClaimant {

  implicit val claimantFormat: Reads[TCClaimant] = (
    (JsPath \ "qualifying").read[Boolean] and
      (JsPath \ "isPartner").read[Boolean] and
        (JsPath \ "claimantDisability").read[TCDisability] and
          ((JsPath \ "doesNotTaper").read[Boolean] or Reads.pure(false))
    )(TCClaimant.apply _)
}

case class TCHouseholdElements(basic: Boolean = false,
                               hours30: Boolean = false,
                               childcare: Boolean = false,
                               loneParent: Boolean = false,
                               secondParent: Boolean = false,
                               family: Boolean = false
                              )

object TCHouseholdElements {
  implicit val householdElementsFormat: Reads[TCHouseholdElements] = Json.reads[TCHouseholdElements]
}
