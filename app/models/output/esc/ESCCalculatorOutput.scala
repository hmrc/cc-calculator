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
case class ESCCalculatorOutput(
                                from: LocalDate,
                                until: LocalDate,
                                totalSavings: ESCSavings,
                                taxYears: List[ESCTaxYear]
                           )

object ESCCalculatorOutput extends CCFormat {
  implicit val escCalculationWrites: Writes[ESCCalculatorOutput] = Json.writes[ESCCalculatorOutput]
}

case class ESCTaxYear(
                       from: LocalDate,
                       until: LocalDate,
                       totalSavings: ESCSavings,
                       claimants: List[ESCClaimant]
                    )

object ESCTaxYear extends CCFormat {
  implicit val TaxYearWrites: Writes[ESCTaxYear] = Json.writes[ESCTaxYear]
}

case class ESCSavings(
                    totalSaving: BigDecimal = BigDecimal(0.00),
                    taxSaving: BigDecimal = BigDecimal(0.00),
                    niSaving: BigDecimal = BigDecimal(0.00)
                    )

object ESCSavings extends CCFormat {
  implicit val SavingsWrites : Writes[ESCSavings] = Json.writes[ESCSavings]
}

case class ESCClaimant(
                        qualifying: Boolean = false,
                        eligibleMonthsInTaxYear: Int,
                        isPartner: Boolean = false,
                        income: ESCIncome,
                        vouchers: Boolean = false,
                        escAmount: BigDecimal = BigDecimal(0.00),
                        escAmountPeriod: Periods.Period,
                        escStartDate: LocalDate,
                        savings: ESCSavings,
                        maximumRelief: BigDecimal = BigDecimal(0.00),
                        maximumReliefPeriod: Periods.Period,
                        taxAndNIBeforeSacrifice: ESCTaxAndNi,
                        taxAndNIAfterSacrifice: ESCTaxAndNi
                     )

object ESCClaimant extends CCFormat {
  implicit val claimantWrites: Writes[ESCClaimant] = Json.writes[ESCClaimant]
}

case class ESCIncome(
                   taxablePay: BigDecimal = BigDecimal(0.00),
                   gross: BigDecimal = BigDecimal(0.00),
                   taxCode: String = "",
                   niCategory: String = ""
                   )

object ESCIncome extends CCFormat {
  implicit val IncomeWrites : Writes[ESCIncome] = Json.writes[ESCIncome]
}

case class ESCTaxAndNi(
  taxPaid: BigDecimal = BigDecimal(0.00),
  niPaid: BigDecimal = BigDecimal(0.00)
 )

object ESCTaxAndNi extends CCFormat {
  implicit val taxAndNIWrites: Writes[ESCTaxAndNi] = Json.writes[ESCTaxAndNi]
}
