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

package calculators

import java.text.NumberFormat
import java.util.Locale
import models.input.APIModels.Request
import models.output.OutputAPIModel.AwardPeriod
import org.joda.time.{Days, LocalDate}
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

trait CCCalculatorHelper {
  /**
   * Unformatted:   5.465068
   * Formatted:      .46
   */
  def round(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_UP)

  def roundToPound(value: BigDecimal): BigDecimal = value.setScale(0, RoundingMode.HALF_UP)

  def roundup (value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.UP)

  def roundDownToThreeDigits(value: BigDecimal): BigDecimal = value.setScale(3, RoundingMode.DOWN)
  def roundDownToTwoDigits(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_EVEN)

  // normalise the monetary amount per quarter quarter
  def amountToQuarterlyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
    fromPeriod match {
      case Periods.Weekly => (cost * 52) / 4
      case Periods.Fortnightly => (cost * 26) / 4
      case Periods.Monthly => cost * 3
      case Periods.Quarterly => cost
      case Periods.Yearly => cost / 4
      case _ => 0.00 //error
    }
  }

  def annualAmountToPeriod(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
    fromPeriod match {
      case Periods.Weekly => cost / 52
      case Periods.Fortnightly => cost / 26
      case Periods.Monthly => cost / 12
      case Periods.Quarterly => cost / 4
      case Periods.Yearly => cost
      case _ => 0.00 //error
    }
  }

  def monthlyAmountToPeriod(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
    fromPeriod match {
      case Periods.Weekly => (cost * 12) / 52
      case Periods.Fortnightly => (cost * 12) / 26
      case Periods.Monthly => cost
      case Periods.Quarterly => cost * 4
      case Periods.Yearly => cost * 12
      case _ => 0.00 //error
    }
  }

  def daysBetween(fromDate: LocalDate, toDate: LocalDate): Int = {
    Days.daysBetween(fromDate, toDate).getDays
  }

}

trait CCCalculator {

  val calculator: CCCalculatorService

  protected trait CCCalculatorRules {

  }

  trait CCCalculatorService extends CCCalculatorRules with CCCalculatorHelper{

    def award(request : Request) : Future[AwardPeriod]

  }

}
