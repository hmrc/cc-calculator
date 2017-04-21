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

import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import play.api.i18n.Messages
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, Reads}
import utils._


/**
 * Created by user on 18/06/15.
 */
case class ESCEligibility(
                          taxYears: List[TaxYear]
                          )

object ESCEligibility extends ESCConfig {
  implicit val escEligibilityReads : Reads[ESCEligibility] =
      (JsPath \ "taxYears").read[List[TaxYear]].map { taxYears =>
        ESCEligibility(taxYears)
      }.filter(ValidationError(Messages("cc.calc.invalid.number.of.ty")))(ty => ty.taxYears.length >= lowerTaxYearsLimitValidation)
}

case class TaxYear(
                    startDate: LocalDate,
                    endDate: LocalDate,
                    periods: List[ESCPeriod]
                    )

object TaxYear extends CCFormat with ESCConfig {
  implicit val taxYearReads: Reads[TaxYear] = (
    (JsPath \ "startDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "endDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "periods").read[List[ESCPeriod]].filter(
        ValidationError(Messages("cc.calc.invalid.number.of.periods"))
        )(periods => periods.length >= lowerPeriodsLimitValidation)
    )(TaxYear.apply _)
}

case class ESCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      claimants: List[Claimant]
                      )

object ESCPeriod extends CCFormat with ESCConfig {
  implicit val periodReads : Reads[ESCPeriod] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[Claimant]].filter(
        ValidationError(Messages("cc.calc.invalid.number.of.claimants"))
        )(claimants => claimants.length >= lowerClaimantsLimitValidation)
    )(ESCPeriod.apply _)
}

//gross and taxablePay are annual amounts
case class Income(
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
        }
        else {
          revisedPersonalAllowance
        }
      case _ =>  defaultPersonalAllowance
    }
  }
}

object Income {
  implicit val incomeReads : Reads[Income] = (
    (JsPath \ "taxablePay").read[BigDecimal].filter(ValidationError(Messages("cc.calc.taxable.pay.less.than.0")))(income => income >= BigDecimal(0.00)) and
      (JsPath \ "gross").read[BigDecimal].filter(ValidationError(Messages("cc.calc.gross.amount.less.than.0")))(income => income >= BigDecimal(0.00)) and
        (JsPath \ "taxCode").read[String].orElse(Reads.pure("")) and
          (JsPath \ "niCategory").read[String].orElse(Reads.pure(""))
    )(Income.apply _)
}

case class ClaimantElements(
                             // claimants qualification is determined by employer providing esc and children's qualification
                             // (if there is at least 1 qualifying child)
                             vouchers : Boolean = false
                             )

object ClaimantElements {
  implicit val claimantReads : Reads[ClaimantElements] = (JsPath \ "vouchers").read[Boolean].orElse(
    Reads.pure(false)
  ).map { vouchers =>
    ClaimantElements(vouchers)
  }
}

case class Claimant (
                     qualifying: Boolean = false,
                     isPartner: Boolean = false,
                     location: String,
                     eligibleMonthsInPeriod: Int,
                     income: Income,
                     elements: ClaimantElements,
                     escStartDate: LocalDate,
                     escAmount: BigDecimal = BigDecimal(0.00),
                     escAmountPeriod: Periods.Period
                     ) extends CCFormat
{
  def isESCStartDateBefore2011 : Boolean = {
    //returns true if ESC start date is before 6th April 2011
    val formatter = DateTimeFormat.forPattern(datePattern)
    val date2011 = LocalDate.parse("2011-04-06", formatter)
    escStartDate.isBefore(date2011)
  }
}

object Claimant extends CCFormat with ESCConfig {
  implicit val claimantReads : Reads[Claimant] = (
    (JsPath \ "qualifying").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "location").read[String] and
          (JsPath \ "eligibleMonthsInPeriod").read[Int].filter(ValidationError(Messages("cc.calc.invalid.number.of.months"))
                                                        )(months => months >= lowerMonthsLimitValidation && months < upperMonthsLimitValidation) and
            (JsPath \ "income").read[Income] and
              (JsPath \ "elements").read[ClaimantElements] and
                (JsPath \ "escStartDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
                  (JsPath \ "escAmount").read[BigDecimal].filter(ValidationError(Messages("cc.calc.voucher.amount.less.than.0"))
                                                          )(income => income >= BigDecimal(0.00)) and
                    (JsPath \ "escAmountPeriod").read[Periods.Period]
    )(Claimant.apply _)
}
