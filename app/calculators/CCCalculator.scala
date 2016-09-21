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

package calculators

import java.text.NumberFormat
import java.util.Locale

import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import org.joda.time.{Days, LocalDate}
import play.api.Logger
import utils.Periods

import scala.concurrent.Future
import scala.math.BigDecimal.RoundingMode


/**
 * Created by adamconder on 08/06/15.
 */

/**
 *
 * Calculator must be instantiated
 *
 * object ExampleCalculator extends ExampleCalculator
 *
 * trait ExampleCalculator extends CCCalculator {
 * val calculator = new ExampleCalculatorClient
 *
 * class ExampleCalculatorClient extends CCCalculatorCommon {
 * // service specific code
 * }
 * }
 */

trait CCCalculator {

  val calculator: CCCalculatorService

  protected trait CCCalculatorRules {

  }

  trait CCCalculatorService extends CCCalculatorRules {

    def award(request : Request) : Future[AwardPeriod]

    /**
     * Unformatted:   5.465068
     * Formatted:      .46
     */
    def round(value: BigDecimal) = value.setScale(2, RoundingMode.HALF_UP)

    def roundToPound(value : BigDecimal) = value.setScale(0, RoundingMode.HALF_UP)

    def roundup (value: BigDecimal) = value.setScale(2, RoundingMode.UP)

    def roundDownToThreeDigits(value : BigDecimal) = value.setScale(3, RoundingMode.DOWN)

    def verifyPenceDifference(amount: BigDecimal, originalSpend: BigDecimal) : (BigDecimal, Boolean) = {

      if (amount > originalSpend) {
        val difference: BigDecimal = amount - originalSpend
        if (difference equals BigDecimal(0.01)) {
          // minus the difference
          (reduceBy1Pence(amount), true)
        } else {
          (amount, false)
        }
      } else {
        (amount, false)
      }
    }

    def reduceBy1Pence(amount: BigDecimal): BigDecimal = {
      amount - BigDecimal(0.01)
    }

    // normalise the monetary amount per quarter quarter
    def amountToQuarterlyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => (cost * 52) / 4
        case Periods.Fortnightly => (cost * 26) / 4
        case Periods.Monthly => cost * 3
        case Periods.Quarterly => cost
        case Periods.Yearly => cost / 4
        case _ => 0.00 //error
      }
      amount
    }


    def amountToWeeklyAmount(cost: BigDecimal, fromPeriod : Periods.Period) : BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => cost
        case Periods.Fortnightly => cost / 2
        case Periods.Monthly => (cost * 12) / 52
        case Periods.Quarterly => (cost * 4) / 52
        case Periods.Yearly => cost / 52
        case _ => 0.00 //error
      }
      amount
    }

    def amountToMonthlyAmount(cost: BigDecimal, fromPeriod : Periods.Period) : BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => (cost * 52) / 12
        case Periods.Fortnightly => (cost * 26) / 12
        case Periods.Monthly => cost
        case Periods.Quarterly => cost / 4
        case Periods.Yearly => cost / 12
        case _ => 0.00 //error
      }
      amount
    }

    def amountToAnnualAmount(cost: BigDecimal, fromPeriod: Periods.Period) : BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => cost * 52
        case Periods.Fortnightly => cost * 26
        case Periods.Monthly => cost * 12
        case Periods.Quarterly => cost * 4
        case Periods.Yearly => cost
        case _ => 0.00 //error
      }
      amount
    }

    def amountFromPeriodToDaily(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => (cost * 52) / 365
        case Periods.Fortnightly => (cost * 26) / 365
        case Periods.Monthly => (cost * 12) / 365
        case Periods.Quarterly => (cost * 4) / 365
        case Periods.Yearly => cost / 365
        case _ => 0.00 //error
      }
      amount
    }

    def annualAmountToPeriod(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => cost / 52
        case Periods.Fortnightly => cost / 26
        case Periods.Monthly => cost / 12
        case Periods.Quarterly => cost / 4
        case Periods.Yearly => cost
        case _ => 0.00 //error
      }
      amount
    }

    def monthlyAmountToPeriod(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      val amount : BigDecimal = fromPeriod match {
        case Periods.Weekly => (cost * 12) / 52
        case Periods.Fortnightly => (cost * 12) / 26
        case Periods.Monthly => cost
        case Periods.Quarterly => cost * 4
        case Periods.Yearly => cost * 12
        case _ => 0.00 //error
      }
      amount
    }

    // quarterly amount into a period
    def quarterlyAmountToPeriod(cost: BigDecimal, period: Periods.Period): BigDecimal = {
      val amount : BigDecimal = period match {
        case Periods.Weekly => (cost / 52) * 4
        case Periods.Fortnightly => (cost / 26) * 4
        case Periods.Monthly => cost / 3
        case Periods.Quarterly => cost
        case Periods.Yearly => cost * 4
        case _ => 0.00 //error
      }
      amount
    }

    def dailyAmountToPeriod(cost: BigDecimal, toPeriod: Periods.Period) : BigDecimal = {
      val amount: BigDecimal = toPeriod match {
        case Periods.Weekly => (cost * 365) / 52
        case Periods.Fortnightly => (cost * 365) / 26
        case Periods.Monthly => (cost * 365) / 12
        case Periods.Quarterly => (cost * 365) / 4
        case Periods.Yearly => cost * 365
        case _ => 0.00 //error
      }
      amount
    }

    def convertToCurrency(amount: BigDecimal): String = {
      val formatter: NumberFormat = NumberFormat.getCurrencyInstance(Locale.UK)
      formatter.format(amount).trim
    }

    def daysBetween(fromDate: LocalDate, toDate: LocalDate) = {
      val numberOfDays = Days.daysBetween(fromDate, toDate)
      numberOfDays.getDays
    }

  }

}
