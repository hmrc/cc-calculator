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

package models.output.esc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.{CCFormat, Periods}

/**
 * Created by user on 18/06/15.
 */
case class ESCCalculation(
                           from: LocalDate,
                           until: LocalDate,
                           totalSavings: Savings,
                           taxYears: List[TaxYear]
                           )

object ESCCalculation extends CCFormat {
  implicit val escCalculationWrites: Writes[ESCCalculation] = Json.writes[ESCCalculation]
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    totalSavings: Savings,
                    claimants: List[Claimant]
                    )

object TaxYear extends CCFormat {
  implicit val TaxYearWrites: Writes[TaxYear] = Json.writes[TaxYear]
}

case class Savings(
                    totalSaving: BigDecimal = BigDecimal(0.00),
                    taxSaving: BigDecimal = BigDecimal(0.00),
                    niSaving: BigDecimal = BigDecimal(0.00)
                    )

object Savings extends CCFormat {
  implicit val SavingsWrites : Writes[Savings] = Json.writes[Savings]
}

case class Claimant(
                     qualifying: Boolean = false,
                     eligibleMonthsInTaxYear: Int,
                     isPartner: Boolean = false,
                     income: Income,
                     vouchers: Boolean = false,
                     escAmount: BigDecimal = BigDecimal(0.00),
                     escAmountPeriod: Periods.Period,
                     escStartDate: LocalDate,
                     savings: Savings,
                     maximumRelief: BigDecimal = BigDecimal(0.00),
                     maximumReliefPeriod: Periods.Period,
                     taxAndNIBeforeSacrifice: TaxAndNI,
                     taxAndNIAfterSacrifice: TaxAndNI
                     )

object Claimant extends CCFormat {
  implicit val claimantWrites: Writes[Claimant] = Json.writes[Claimant]
}

case class Income(
                   taxablePay: BigDecimal = BigDecimal(0.00),
                   gross: BigDecimal = BigDecimal(0.00),
                   taxCode: String = "",
                   niCategory: String = ""
                   )

object Income extends CCFormat {
  implicit val IncomeWrites : Writes[Income] = Json.writes[Income]
}

case class TaxAndNI(
  taxPaid: BigDecimal = BigDecimal(0.00),
  niPaid: BigDecimal = BigDecimal(0.00)
 )

object TaxAndNI extends CCFormat {
  implicit val taxAndNIWrites: Writes[TaxAndNI] = Json.writes[TaxAndNI]
}
