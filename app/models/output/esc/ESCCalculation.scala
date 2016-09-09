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

package models.output.esc

import org.joda.time.LocalDate
import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json.{Format, JsPath, Json, Writes}
import utils.{CCFormat, Periods}

/**
 * Created by user on 18/06/15.
 */
case class ESCCalculation(
                           from : LocalDate,
                           until : LocalDate,
                           totalSavings : Savings,
                           taxYears : List[TaxYear]
                           )

object ESCCalculation extends CCFormat {
  implicit val escCalculationWrites : Writes[ESCCalculation] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "totalSavings").write[Savings] and
          (JsPath \ "taxYears").write[List[TaxYear]]
    )(unlift(ESCCalculation.unapply))
  }

case class TaxYear(
                    from : LocalDate,
                    until : LocalDate,
                    totalSavings : Savings,
                    claimants : List[Claimant]
                    )

object TaxYear extends CCFormat {
  implicit val TaxYearWrites: Writes[TaxYear] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "totalSavings").write[Savings] and
          (JsPath \ "claimants").write[List[Claimant]]
    )(unlift(TaxYear.unapply))
  }

case class Savings(
                    totalSaving : BigDecimal = BigDecimal(0.00),
                    taxSaving: BigDecimal = BigDecimal(0.00),
                    niSaving : BigDecimal = BigDecimal(0.00)
                    )

object Savings extends CCFormat {
  implicit val SavingsWrites : Writes[Savings] = (
    (JsPath \ "totalSaving").write[BigDecimal] and
      (JsPath \ "taxSaving").write[BigDecimal] and
        (JsPath \ "niSaving").write[BigDecimal]
    )(unlift(Savings.unapply))
  }

case class Claimant(
                     qualifying : Boolean = false,
                     eligibleMonthsInTaxYear : Int,
                     isPartner : Boolean = false,
                     income : Income,
                     elements : ClaimantElements,
                     escAmount : BigDecimal = BigDecimal(0.00),
                     escAmountPeriod : Periods.Period,
                     escStartDate : LocalDate,
                     savings : Savings,
                     maximumRelief : BigDecimal = BigDecimal(0.00),
                     maximumReliefPeriod : Periods.Period,
                     taxAndNIBeforeSacrifice : TaxAndNI,
                     taxAndNIAfterSacrifice : TaxAndNI
                     )

object Claimant extends CCFormat {
  implicit val claimantWrites: Writes[Claimant] = (
    (JsPath \ "qualifying").write[Boolean] and
      (JsPath \ "eligibleMonthsInTaxYear").write[Int] and
        (JsPath \ "isPartner").write[Boolean] and
          (JsPath \ "income").write[Income] and
            (JsPath \ "elements").write[ClaimantElements] and
              (JsPath \ "escAmount").write[BigDecimal] and
                (JsPath \ "escAmountPeriod").write[Periods.Period] and
                  (JsPath \ "escStartDate").write[LocalDate](jodaLocalDateWrites(datePattern)) and
                    (JsPath \ "savings").write[Savings] and
                      (JsPath \ "maximumRelief").write[BigDecimal] and
                        (JsPath \ "maximumReliefPeriod").write[Periods.Period] and
                          (JsPath \ "taxAndNIBeforeSacrifice").write[TaxAndNI] and
                            (JsPath \ "taxAndNIAfterSacrifice").write[TaxAndNI]
    )(unlift(Claimant.unapply))
  }

case class Income(
                   taxablePay: BigDecimal = BigDecimal(0.00),
                   gross: BigDecimal = BigDecimal(0.00),
                   taxCode: String = "",
                   niCategory: String = ""
                   )

object Income extends CCFormat {
  implicit val IncomeWrites : Writes[Income]=(
    (JsPath \ "taxablePay").write[BigDecimal] and
      (JsPath \ "gross").write[BigDecimal] and
        (JsPath \ "taxCode").write[String] and
          (JsPath \ "niCategory").write[String]
    )(unlift(Income.unapply))
}

case class ClaimantElements(
    vouchers : Boolean = false
  )

object ClaimantElements extends CCFormat {
  implicit val ClaimantElementsWrites : Format[ClaimantElements] = Json.format[ClaimantElements]
}

case class TaxAndNI(
  taxPaid : BigDecimal = BigDecimal(0.00),
  niPaid : BigDecimal = BigDecimal(0.00)
 )

object TaxAndNI extends CCFormat {
  implicit val taxAndNIWrites: Writes[TaxAndNI] = (
    (JsPath \ "taxPaid").write[BigDecimal] and
      (JsPath \ "niPaid").write[BigDecimal]
    )(unlift(TaxAndNI.unapply))
}
