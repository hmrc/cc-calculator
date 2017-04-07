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

import models.input.APIModels.Request
import models.input.tfc.{Child, TFCEligibility}
import models.output.OutputAPIModel.AwardPeriod
import models.output.tfc._
import org.joda.time.LocalDate
import play.api.Logger
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import play.api.i18n.Messages
import utils.{Periods, TFCTaxYearConfig}

import scala.concurrent.Future
import scala.util.Success

object TFCCalculator extends TFCCalculator

trait TFCCalculator extends CCCalculator {

  val calculator = new TFCCalculatorService

  class TFCCalculatorService extends CCCalculatorService {

    import scala.concurrent.ExecutionContext.Implicits.global

    def getHouseholdContribution(periods: List[TFCPeriod]) : Contribution = {
      val householdTotalParentContribution: BigDecimal =
        periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.parent)
      val householdTotalGovernmentContribution: BigDecimal =
        periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.government)
      val householdTotalChildcareSpend: BigDecimal =
        periods.foldLeft(BigDecimal(0.00))((acc, TFCPeriod) => acc + TFCPeriod.periodContribution.totalChildCareSpend)

      Contribution(
        parent = round(householdTotalParentContribution),
        government = round(householdTotalGovernmentContribution),
        totalChildCareSpend = round(householdTotalChildcareSpend)
      )
    }


    def getChildQualifyingDaysInTFCPeriod(from: Option[LocalDate], until: Option[LocalDate]): Int = {
      (from, until) match {
        case (null, Some(u)) =>
          Logger.warn("TFCCalculator.TFCCalculatorService.getChildQualifyingDaysInTFCPeriod Exception - from date is null")
          throw new IllegalArgumentException(Messages("cc.scheme.config.from.date"))
        case (Some(f), null) =>
          Logger.warn("TFCCalculator.TFCCalculatorService.getChildQualifyingDaysInTFCPeriod Exception - until date is null")
          throw new IllegalArgumentException(Messages("cc.scheme.config.until.date"))
        case (null,null) =>
          Logger.warn("TFCCalculator.TFCCalculatorService.getChildQualifyingDaysInTFCPeriod Exception - from and until dates are null")
          throw new IllegalArgumentException(Messages("cc.scheme.config.from.until.date"))
        case (Some(f), Some(u)) => daysBetween(f,u)
        case (_, _) =>
          Logger.warn("TFCCalculator.TFCCalculatorService.getChildQualifyingDaysInTFCPeriod Exception - from and until dates are incorrect")
          throw new IllegalArgumentException
      }
    }

    def getMaximumTopup(child : models.input.tfc.Child, tfcTaxYearConfig : TFCTaxYearConfig): BigDecimal = {
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
      for (period <- periods) yield {
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

    def getPeriodContribution(Children : List[OutputChild]) : Contribution = {
      val periodParentContribution: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.parent)
      val periodGovernmentContribution: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.government)
      val periodTotalChildcareSpend: BigDecimal = Children.foldLeft(BigDecimal(0.00))((acc, child) => acc + child.childContribution.totalChildCareSpend)

      Contribution(
        parent = round(periodParentContribution),
        government = round(periodGovernmentContribution),
        totalChildCareSpend = round(periodTotalChildcareSpend)
      )
    }

    def getOutputChildren(period: models.input.tfc.TFCPeriod): List[OutputChild] = {
      for(child <- period.children) yield {
        OutputChild(
          id = child.id,
          name = child.name,
          childCareCost = child.childcareCost,
          childContribution = getChildContribution(child, period.configRule, daysBetween(period.from, period.until), period.periodEligibility),
          timeToMaximizeTopUp = 0,
          failures = List()
        )
      }

    }

    def getChildContribution(child: Child, tfcTaxYearConfig: TFCTaxYearConfig, noOfDaysInAPeriod: Int, periodEligible: Boolean): Contribution = {

      val totalChildCareSpend: BigDecimal  = getChildCareCostForPeriod(child)

      val governmentContribution: BigDecimal = {
        child.qualifying match {
          case true => (getMaximumTopup(child, tfcTaxYearConfig)/noOfDaysInAPeriod) * (getChildQualifyingDaysInTFCPeriod(child.from, child.until))
          case _ => BigDecimal(0.00)
        }
      }

      val governmentContributionRounded = roundup(roundDownToThreeDigits(governmentContribution))
      if(periodEligible) {
        Contribution(
          parent = totalChildCareSpend - governmentContributionRounded,
          government = governmentContributionRounded,
          totalChildCareSpend = totalChildCareSpend
        )
      } else { //if both parent and child not eligible in 3 months period government contribution is zero
        Contribution(
          parent = totalChildCareSpend,
          government = 0,
          totalChildCareSpend = totalChildCareSpend
        )
      }
    }

     def getChildCareCostForPeriod(child: Child): BigDecimal = amountToQuarterlyAmount(child.childcareCost, Periods.Monthly)

     def getTopUpPercentForChildCareCost(child: Child, tfcTaxYearConfig: TFCTaxYearConfig): BigDecimal =
       ((tfcTaxYearConfig.topUpPercent * getChildCareCostForPeriod(child)) / 100)

    override def award(request : Request) : Future[AwardPeriod] = {
      def getTFCCalculation(eligibility: TFCEligibility): TFCCalculation = {
        TFCCalculation(
          from = eligibility.from,
          until = eligibility.until,
          householdContribution = getHouseholdContribution(getCalculatedTFCPeriods(eligibility.periods)),
          numberOfPeriods = eligibility.periods.length.toShort,
          periods = getCalculatedTFCPeriods(eligibility.periods)
        )
      }

      Future {
        request.getTFCEligibility match {
          case Success(result) if(result.householdEligibility) => AwardPeriod(tfc = Some(getTFCCalculation(result)))
          case _ => AwardPeriod()
        }
      }
    }
  }
}
