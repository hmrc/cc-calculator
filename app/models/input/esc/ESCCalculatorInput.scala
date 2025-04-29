/*
 * Copyright 2024 HM Revenue & Customs
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

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import config.ConfigConstants._
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, JsonValidationError, Reads}
import utils.{Periods, _}

case class ESCCalculatorInput(taxYears: List[ESCTaxYear], location: String)

object ESCCalculatorInput extends MessagesObject {

  implicit val escEligibilityReads: Reads[ESCCalculatorInput] =
    (JsPath \ "taxYears")
      .read[List[ESCTaxYear]]
      .filter(JsonValidationError("Please provide at least 1 Tax Year"))(taxYears =>
        taxYears.length >= lowerTaxYearsLimitValidation
      )
      .and((JsPath \ "location").read[String])(ESCCalculatorInput.apply _)

}

case class ESCTaxYear(from: LocalDate, until: LocalDate, periods: List[ESCPeriod])

object ESCTaxYear extends MessagesObject {

  implicit val taxYearReads: Reads[ESCTaxYear] =
    (JsPath \ "from")
      .read[LocalDate]
      .and((JsPath \ "until").read[LocalDate])
      .and(
        (JsPath \ "periods")
          .read[List[ESCPeriod]]
          .filter(
            JsonValidationError("Please provide at least 1 Period")
          )(periods => periods.length >= lowerPeriodsLimitValidation)
      )(ESCTaxYear.apply _)

}

case class ESCPeriod(from: LocalDate, until: LocalDate, claimants: List[ESCClaimant], children: List[Child])

object ESCPeriod extends MessagesObject {

  implicit val periodReads: Reads[ESCPeriod] =
    (JsPath \ "from")
      .read[LocalDate]
      .and((JsPath \ "until").read[LocalDate])
      .and(
        (JsPath \ "claimants")
          .read[List[ESCClaimant]]
          .filter(JsonValidationError("Please provide at least 1 claimant"))(claimants =>
            claimants.length >= lowerClaimantsLimitValidation
          )
      )
      .and((JsPath \ "children").read[List[Child]])(ESCPeriod.apply _)

}

case class Child(
    qualifying: Boolean = false,
    childCareCost: BigDecimal,
    childCareCostPeriod: Periods.Period = Periods.Monthly
)

object Child {

  def childSpendValidation(cost: BigDecimal): Boolean =
    cost >= BigDecimal(0.00)

  implicit val childReads: Reads[Child] =
    (JsPath \ "qualifying")
      .read[Boolean]
      .and(
        (JsPath \ "childCareCost")
          .read[BigDecimal]
          .filter(JsonValidationError("Childcare Spend cost should not be less than 0.00"))(x =>
            childSpendValidation(x)
          )
      )
      .and((JsPath \ "childCareCostPeriod").read[Periods.Period])(Child.apply _)

}

//gross and taxablePay are annual amounts
case class ESCTotalIncome(
    taxablePay: BigDecimal = BigDecimal(0.00),
    gross: BigDecimal = BigDecimal(0.00),
    taxCode: String = "",
    niCategory: String = ""
) {

  def adjustPersonalAllowance(defaultPersonalAllowance: BigDecimal): BigDecimal =
    gross match {
      case amount if amount > BigDecimal(100000.00) =>
        val revisedPersonalAllowance =
          reducePersonalAllowanceBy1PoundForEvery2PoundsYouGetOver100000(defaultPersonalAllowance, amount)
        if (revisedPersonalAllowance < 0) {
          BigDecimal(0.00)
        } else {
          revisedPersonalAllowance
        }
      case _ => defaultPersonalAllowance
    }

  private def reducePersonalAllowanceBy1PoundForEvery2PoundsYouGetOver100000(
      defaultPersonalAllowance: BigDecimal,
      amount: BigDecimal
  ) =
    defaultPersonalAllowance - (amount - BigDecimal(100000.00)) / 2

}

case class ESCIncome(
    employmentIncome: Option[BigDecimal] = None,
    pension: Option[BigDecimal] = None,
    taxCode: Option[String] = None
)

object ESCIncome {
  implicit val incomeRead: Reads[ESCIncome] = Json.reads[ESCIncome]
}

case class ESCClaimant(
    qualifying: Boolean = false,
    isPartner: Boolean = false,
    eligibleMonthsInPeriod: Int,
    currentIncome: Option[ESCIncome],
    vouchers: Boolean = false,
    escStartDate: LocalDate
) extends MessagesObject {

  def isESCStartDateBefore2011: Boolean = {
    // returns true if ESC start date is before 6th April 2011
    val formatter = DateTimeFormatter.ofPattern(datePattern)
    val date2011  = LocalDate.parse("2011-04-06", formatter)
    escStartDate.isBefore(date2011)
  }

  val income: ESCTotalIncome = {
    val (empIncome, pension, inputTaxCode) = getTotalIncome(currentIncome)
    ESCTotalIncome(
      taxablePay = empIncome.getOrElse(defaultAmount) - (pension.getOrElse(defaultAmount) * noOfMonths),
      gross = empIncome.getOrElse(defaultAmount),
      taxCode = inputTaxCode.getOrElse("")
    )
  }

  private def determineIncomeElems(income: Option[ESCIncome]) = income match {
    case Some(x) => (x.employmentIncome, x.pension, x.taxCode)
    case _       => (None, None, None)
  }

  private def getTotalIncome(currentIncome: Option[ESCIncome]) = {
    val (emp, pension, taxCode) = determineIncomeElems(currentIncome)

    (
      emp,
      pension,
      taxCode
    )
  }

}

object ESCClaimant extends MessagesObject {

  implicit val claimantReads: Reads[ESCClaimant] =
    (JsPath \ "qualifying")
      .read[Boolean]
      .orElse(Reads.pure(false))
      .and((JsPath \ "isPartner").read[Boolean].orElse(Reads.pure(false)))
      .and(
        (JsPath \ "eligibleMonthsInPeriod")
          .read[Int]
          .filter(JsonValidationError("Number of months should not be less than 0 and not more than 99"))(months =>
            months >= lowerMonthsLimitValidation && months < upperMonthsLimitValidation
          )
      )
      .and((JsPath \ "currentIncome").readNullable[ESCIncome])
      .and((JsPath \ "vouchers").read[Boolean])
      .and((JsPath \ "escStartDate").read[LocalDate])(ESCClaimant.apply _)

}
