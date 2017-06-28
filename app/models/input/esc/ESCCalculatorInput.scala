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
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.data.validation.ValidationError
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, Json, Reads}
import utils._

case class ESCCalculatorInput(
                          taxYears: List[ESCTaxYear]
                          )

object ESCCalculatorInput extends ESCConfig with MessagesObject {
  implicit val escEligibilityReads : Reads[ESCCalculatorInput] =
      (JsPath \ "taxYears").read[List[ESCTaxYear]].map { taxYears =>
        ESCCalculatorInput(taxYears)
      }.filter(ValidationError(messages("cc.calc.invalid.number.of.ty")))(ty => ty.taxYears.length >= lowerTaxYearsLimitValidation)
}

case class ESCTaxYear(
                    startDate: LocalDate,
                    endDate: LocalDate,
                    periods: List[ESCPeriod]
                    )

object ESCTaxYear extends CCFormat with ESCConfig with MessagesObject {
  implicit val taxYearReads: Reads[ESCTaxYear] = (
    (JsPath \ "startDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "endDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "periods").read[List[ESCPeriod]].filter(
        ValidationError(messages("cc.calc.invalid.number.of.periods"))
        )(periods => periods.length >= lowerPeriodsLimitValidation)
    )(ESCTaxYear.apply _)
}

case class ESCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      claimants: List[ESCClaimant]
                      )

object ESCPeriod extends CCFormat with ESCConfig with MessagesObject {
  implicit val periodReads : Reads[ESCPeriod] = (
    (JsPath \ "from").read[LocalDate](jodaLocalDateReads(datePattern)) and
      (JsPath \ "until").read[LocalDate](jodaLocalDateReads(datePattern)) and
        (JsPath \ "claimants").read[List[ESCClaimant]].filter(
        ValidationError(messages("cc.calc.invalid.number.of.claimants"))
        )(claimants => claimants.length >= lowerClaimantsLimitValidation)
    )(ESCPeriod.apply _)
}

//gross and taxablePay are annual amounts
case class ESCTotalIncome(
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

case class ESCIncome(
                   employmentIncome : Option[BigDecimal] = None,
                   pension : Option[BigDecimal] = None
                 )

object ESCIncome {
  implicit val formats = Json.format[ESCIncome]
}

case class ESCClaimant(
                      qualifying: Boolean = false,
                      isPartner: Boolean = false,
                      location: String,
                      eligibleMonthsInPeriod: Int,
                      previousIncome: Option[ESCIncome],
                      currentIncome: Option[ESCIncome],
                      vouchers: Boolean = false,
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

  val income: ESCTotalIncome = {
    val (empIncome, pension) = getTotalIncome(previousIncome, currentIncome)
    ESCTotalIncome(
      taxablePay = (empIncome.getOrElse(defaultAmount) - (pension.getOrElse(defaultAmount) * noOfMonths)),
      gross = empIncome.getOrElse(defaultAmount),
      taxCode = "",
      niCategory = ""
    )
  }

  private def determineIncomeElems(income: Option[ESCIncome]) = income  match {
    case Some(x) => (x.employmentIncome, x.pension)
    case _ => (None, None)
  }

  private def getTotalIncome(previousIncome : Option[ESCIncome], currentIncome: Option[ESCIncome]) = {
    val (empPrevious, pensionPrevious) = determineIncomeElems(previousIncome)
    val (emp, pension) = determineIncomeElems(currentIncome)

    (if(emp.isDefined) emp else empPrevious,
      if(pension.isDefined) pension else pensionPrevious)
  }
}

object ESCClaimant extends CCFormat with ESCConfig with MessagesObject {
  implicit val claimantReads : Reads[ESCClaimant] = (
    (JsPath \ "qualifying").read[Boolean].orElse(Reads.pure(false)) and
      (JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)) and
        (JsPath \ "location").read[String] and
          (JsPath \ "eligibleMonthsInPeriod").read[Int].filter(ValidationError(messages("cc.calc.invalid.number.of.months"))
                                                        )(months => months >= lowerMonthsLimitValidation && months < upperMonthsLimitValidation) and
            (JsPath \ "previousIncome").readNullable[ESCIncome] and
            (JsPath \ "currentIncome").readNullable[ESCIncome] and
              (JsPath \ "vouchers").read[Boolean] and
                (JsPath \ "escStartDate").read[LocalDate](jodaLocalDateReads(datePattern)) and
                  (JsPath \ "escAmount").read[BigDecimal].filter(ValidationError(messages("cc.calc.voucher.amount.less.than.0"))
                                                          )(income => income >= BigDecimal(0.00)) and
                    (JsPath \ "escAmountPeriod").read[Periods.Period]
    )(ESCClaimant.apply _)
}