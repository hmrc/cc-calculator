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

package models.input.esc

import config.ConfigConstants._
import models.input.tc.Child.{childSpendValidation, messages}
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsPath, Json, Reads}
import utils.Periods
import org.joda.time.LocalDate
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import utils._

case class ESCEligibility(
                          escTaxYears: List[TaxYear],
                          location: String
                          )

object ESCEligibility extends ESCConfig with MessagesObject {
  implicit val escEligibilityReads : Reads[ESCEligibility] = (
      (JsPath \ "escTaxYears").read[List[TaxYear]].filter(ValidationError(messages("cc.calc.invalid.number.of.ty")))
      (taxYears => taxYears.length >= lowerTaxYearsLimitValidation) and
        (JsPath \ "location").read[String]
    )(ESCEligibility.apply _)
}

case class TaxYear(
                    startDate: LocalDate,
                    endDate: LocalDate,
                    periods: List[ESCPeriod]
                    )

object TaxYear extends CCFormat with ESCConfig with MessagesObject {
  implicit val taxYearReads: Reads[TaxYear] = (
    (JsPath \ "startDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "endDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "periods").read[List[ESCPeriod]].filter(
        ValidationError(messages("cc.calc.invalid.number.of.periods"))
        )(periods => periods.length >= lowerPeriodsLimitValidation)
    )(TaxYear.apply _)
}

case class ESCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      claimants: List[Claimant],
                      children: List[Child]
                      )

object ESCPeriod extends CCFormat with ESCConfig with MessagesObject {
  implicit val periodReads : Reads[ESCPeriod] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[Claimant]].filter(
          ValidationError(messages("cc.calc.invalid.number.of.claimants"))
        )(claimants => claimants.length >= lowerClaimantsLimitValidation) and
          (JsPath \ "children").read[List[Child]]
    )(ESCPeriod.apply _)
}

case class Child(
                   qualifying: Boolean = false,
                   childCareCost: BigDecimal,
                   childCareCostPeriod: Periods.Period = Periods.Monthly
                   ) {
}

object Child {

  def childSpendValidation(cost: BigDecimal) : Boolean = {
    cost >= BigDecimal(0.00)
  }

  implicit val childReads : Reads[Child] = (
    (JsPath \ "qualifying").read[Boolean] and
      (JsPath \ "childcareCost").read[BigDecimal].filter(
        ValidationError(messages("cc.calc.childcare.spend.too.low"))
      )(x => childSpendValidation(x)) and
  (JsPath \ "childCareCostPeriod").read[Periods.Period]
  )(Child.apply _)

}

//gross and taxablePay are annual amounts
case class TotalIncome(
                   taxablePay: BigDecimal = BigDecimal(0.00),
                   gross: BigDecimal = BigDecimal(0.00),
                   taxCode: String = "",
                   niCategory: String = ""
                   ) {
  def adjustPersonalAllowance(defaultPersonalAllowance :BigDecimal) : BigDecimal = {
    gross match {
      case amount if amount > BigDecimal(100000.00) =>
        val revisedPersonalAllowance = defaultPersonalAllowance - (amount - BigDecimal(100000.00)) / 2
        if(revisedPersonalAllowance < 0) {
          BigDecimal(0.00)
        } else {
          revisedPersonalAllowance
        }
      case _ =>  defaultPersonalAllowance
    }
  }
}


case class Income(
                   employmentIncome : Option[BigDecimal] = None,
                   pension : Option[BigDecimal] = None
                 )

object Income {
  implicit val incomeRead: Reads[Income] = Json.reads[Income]
}

case class Claimant (
                     qualifying: Boolean = false,
                     isPartner: Boolean = false,
                     eligibleMonthsInPeriod: Int,
                     previousIncome: Option[Income],
                     currentIncome: Option[Income],
                     vouchers: Boolean = false,
                     escStartDate: LocalDate
                     ) extends CCFormat
{
  def isESCStartDateBefore2011 : Boolean = {
    //returns true if ESC start date is before 6th April 2011
    val formatter = DateTimeFormat.forPattern(datePattern)
    val date2011 = LocalDate.parse("2011-04-06", formatter)
    escStartDate.isBefore(date2011)
  }

  val income: TotalIncome = {
    val (empIncome, pension) = getTotalIncome(previousIncome, currentIncome)
    TotalIncome(
      taxablePay = (empIncome.getOrElse(defaultAmount) - (pension.getOrElse(defaultAmount) * noOfMonths)),
      gross = empIncome.getOrElse(defaultAmount),
      taxCode = "",
      niCategory = ""
    )
  }

  private def determineIncomeElems(income: Option[Income]) = income  match {
    case Some(x) => (x.employmentIncome, x.pension)
    case _ => (None, None)
  }

  private def getTotalIncome(previousIncome : Option[Income], currentIncome: Option[Income]) = {
    val (empPrevious, pensionPrevious) = determineIncomeElems(previousIncome)
    val (emp, pension) = determineIncomeElems(currentIncome)

    (if(emp.isDefined) emp else empPrevious,
      if(pension.isDefined) pension else pensionPrevious)
  }
}

object Claimant extends CCFormat with ESCConfig with MessagesObject {
  implicit val claimantReads : Reads[Claimant] = (
    (JsPath \ "qualifying").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "eligibleMonthsInPeriod").read[Int].filter(ValidationError(messages("cc.calc.invalid.number.of.months"))
          )(months => months >= lowerMonthsLimitValidation && months < upperMonthsLimitValidation) and
            (JsPath \ "previousIncome").readNullable[Income] and
            (JsPath \ "currentIncome").readNullable[Income] and
              (JsPath \ "vouchers").read[Boolean] and
                (JsPath \ "escStartDate").read[LocalDate](jodaLocalDateReads(datePattern))
    )(Claimant.apply _)
}
