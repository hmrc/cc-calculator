/*
 * Copyright 2022 HM Revenue & Customs
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

import javax.inject.Inject
import models.input.tfc.{TFCCalculatorInput, TFCChild}
import models.output.tfc._
import org.joda.time.LocalDate
import play.api.Logging
import utils.{MessagesObject, TFCConfig, TFCTaxYearConfig}

import scala.concurrent.Future


class TFCCalculator @Inject()(tfcConfig: TFCConfig)
  extends CCCalculatorHelper with MessagesObject with Logging {

  import scala.concurrent.ExecutionContext.Implicits.global

  def getHouseholdContribution(periods: List[TFCPeriod]) : TFCContribution = {
    val householdTotalParentContribution: BigDecimal =
      periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.parent)
    val householdTotalGovernmentContribution: BigDecimal =
      periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.government)
    val householdTotalChildcareSpend: BigDecimal =
      periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.totalChildCareSpend)

    TFCContribution(
      parent = round(householdTotalParentContribution),
      government = round(householdTotalGovernmentContribution),
      totalChildCareSpend = round(householdTotalChildcareSpend)
    )
  }


  def getChildQualifyingDaysInTFCPeriod(from: Option[LocalDate], until: Option[LocalDate]): Int = {
    (from, until) match {
      case (Some(f), Some(u)) => daysBetween(f,u)
      case (_, _) =>
        logger.warn("TFCCalculator.TFCCalculatorService.getChildQualifyingDaysInTFCPeriod Exception - from and until dates are incorrect")
        throw new IllegalArgumentException("unspecified argument: from date until date")
    }
  }

  def getMaximumTopup(child : models.input.tfc.TFCChild, tfcTaxYearConfig : TFCTaxYearConfig): BigDecimal = {
    val maximumTopupConfig = child.getChildDisability match {
      case true => tfcTaxYearConfig.maxGovtContributionForDisabled
      case _ => tfcTaxYearConfig.maxGovtContribution
    }
    getTopUpPercentForChildCareCost(child,tfcTaxYearConfig) match {
      case govContribution if govContribution < maximumTopupConfig => govContribution
      case _ => maximumTopupConfig
    }
  }

  def getCalculatedTFCPeriods(periods: List[models.input.tfc.TFCPeriod]): List[TFCPeriod] = {
    for (periodWithoutConfig <- periods) yield {
      val period = periodWithoutConfig.createNewWithConfig(tfcConfig)
      val outputChildren = getOutputChildren(period)
      val outputPeriod = getPeriodContribution(outputChildren)
      TFCPeriod(
        from = period.from,
        until = period.until,
        periodContribution = outputPeriod,
        children = outputChildren
      )
    }
  }

  def getPeriodContribution(Children : List[TFCOutputChild]) : TFCContribution = {
    val periodParentContribution: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.parent)
    val periodGovernmentContribution: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.government)
    val periodTotalChildcareSpend: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.totalChildCareSpend)

    TFCContribution(
      parent = round(periodParentContribution),
      government = round(periodGovernmentContribution),
      totalChildCareSpend = round(periodTotalChildcareSpend)
    )
  }

  def getOutputChildren(period: models.input.tfc.TFCPeriod): List[TFCOutputChild] = {
    for(child <- period.children) yield {
      TFCOutputChild(
        childCareCost = child.childcareCost,
        childContribution = getChildContribution(child, period.configRule, daysBetween(period.from, period.until), period.periodEligibility)
      )
    }

  }

  def getChildContribution(child: TFCChild, tfcTaxYearConfig: TFCTaxYearConfig, noOfDaysInAPeriod: Int, periodEligible: Boolean): TFCContribution = {

    val totalChildCareSpend: BigDecimal  = getChildCareCostForPeriod(child)

    val governmentContribution: BigDecimal = {
      if(child.qualifying) {
        (getMaximumTopup(child, tfcTaxYearConfig)/noOfDaysInAPeriod) * getChildQualifyingDaysInTFCPeriod(child.from, child.until)
      } else { BigDecimal(0.00) }
    }

    val governmentContributionRounded = roundup(roundDownToThreeDigits(governmentContribution))
    if(periodEligible) {
      TFCContribution(
        parent = totalChildCareSpend - governmentContributionRounded,
        government = governmentContributionRounded,
        totalChildCareSpend = totalChildCareSpend
      )
    } else { //if both parent and child not eligible in 3 months period government contribution is zero
      TFCContribution(
        parent = totalChildCareSpend,
        government = 0,
        totalChildCareSpend = totalChildCareSpend
      )
    }
  }

   def getChildCareCostForPeriod(child: TFCChild): BigDecimal = amountToQuarterlyAmount(child.childcareCost, child.childcareCostPeriod)

   def getTopUpPercentForChildCareCost(child: TFCChild, tfcTaxYearConfig: TFCTaxYearConfig): BigDecimal =
     (tfcTaxYearConfig.topUpPercent * getChildCareCostForPeriod(child)) / 100

  def award(request: TFCCalculatorInput): Future[TFCCalculatorOutput] = {
    if(request.householdEligibility) {
      Future {
        TFCCalculatorOutput(
          householdContribution = getHouseholdContribution(getCalculatedTFCPeriods(request.periods)),
          numberOfPeriods = request.periods.length.toShort,
          periods = getCalculatedTFCPeriods(request.periods)
        )
      }
    }
    else {
      Future {
        TFCCalculatorOutput(
          householdContribution = TFCContribution(parent = 0, government = 0, totalChildCareSpend = 0),
          numberOfPeriods = 0,
          periods = List.empty
        )
      }
    }
  }
}
