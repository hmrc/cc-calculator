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

package calculators

import java.time.LocalDate
import java.time.temporal.ChronoUnit.DAYS
import utils.Periods

import scala.math.BigDecimal.RoundingMode

trait CCCalculatorHelper {

  /** Unformatted: 5.465068 Formatted: .46
    */
  def round(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_UP)

  def roundToPound(value: BigDecimal): BigDecimal = value.setScale(0, RoundingMode.HALF_UP)

  def roundup(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.UP)

  def roundDownToThreeDigits(value: BigDecimal): BigDecimal = value.setScale(3, RoundingMode.DOWN)

  def roundDownToTwoDigits(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_EVEN)

  // normalise the monetary amount per quarter quarter
  def amountToQuarterlyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal =
    fromPeriod match {
      case Periods.Weekly  => (cost * 52) / 4
      case Periods.Monthly => cost * 3
      case Periods.Yearly  => cost / 4
      case _               => 0.00 // error
    }

  def annualAmountToPeriod(cost: BigDecimal, toPeriod: Periods.Period): BigDecimal =
    toPeriod match {
      case Periods.Weekly  => cost / 52
      case Periods.Monthly => cost / 12
      case Periods.Yearly  => cost
      case _               => 0.00 // error
    }

  def monthlyAmountToPeriod(cost: BigDecimal, toPeriod: Periods.Period): BigDecimal =
    toPeriod match {
      case Periods.Weekly  => (cost * 12) / 52
      case Periods.Monthly => cost
      case Periods.Yearly  => cost * 12
      case _               => 0.00 // error
    }

  def amountToAnnualAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal =
    fromPeriod match {
      case Periods.Weekly  => cost * 52
      case Periods.Monthly => cost * 12
      case Periods.Yearly  => cost
      case _               => 0.00 // error
    }

  def amountToMonthlyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal =

    fromPeriod match {
      case Periods.Weekly  => cost * 52 / 12
      case Periods.Monthly => cost
      case Periods.Yearly  => cost / 12
      case _               => 0.00 // error
    }

  def amountToWeeklyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal =
    fromPeriod match {
      case Periods.Weekly  => cost
      case Periods.Monthly => (cost * 12) / 52
      case Periods.Yearly  => cost / 52
      case _               => 0.00 // error
    }

  def amountFromPeriodToDaily(cost: BigDecimal, fromPeriod: Periods.Period, daysInTheYear: Int): BigDecimal = {
    val amount: BigDecimal = fromPeriod match {
      case Periods.Weekly  => (cost * 52) / daysInTheYear
      case Periods.Monthly => (cost * 12) / daysInTheYear
      case Periods.Yearly  => cost / daysInTheYear
      case _               => 0.00 // error
    }
    amount
  }

  def daysBetween(fromDate: LocalDate, toDate: LocalDate): Int =
    DAYS.between(fromDate, toDate).toInt

}
