/*
 * Copyright 2020 HM Revenue & Customs
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
import models.input.tc._
import models.output.tc.{Element, Elements, TCCalculatorOutput, TaxYear}
import org.joda.time.LocalDate
import utils.{Periods, TCConfig, TCTaxYearConfig}

import scala.concurrent.Future

trait TCCalculatorElements extends TCCalculatorTapering {

  def generateMaximumAmountsForPeriod(period: TCPeriod): models.output.tc.Period = {
    val childElement = maxChildElementForPeriod(period)
    val familyElement = maxFamilyElementForPeriod(period)
    val childcareElement = maxChildcareElementForPeriod(period)
    val workingTaxElement = maxWorkElementForPeriod(period) //this contains basic, 30 hour, claimant disability/severe disability + lone/second parent
    models.output.tc.Period(
      from = period.from,
      until = period.until,
      elements = Elements(
        wtcWorkElement = Element(
          maximumAmount = workingTaxElement
        ),
        wtcChildcareElement = Element(
          maximumAmount = childcareElement
        ),
        ctcIndividualElement = Element(
          maximumAmount = childElement
        ),
        ctcFamilyElement = Element(
          maximumAmount = familyElement
        )
      )
    )
  }

  def maxFamilyElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.householdElements.family) {
      val familyElementMaximumAmount = config(period).ctc.familyElement
      amountForDateRange(familyElementMaximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def maxChildElementForPeriod(period: TCPeriod): BigDecimal = {
    period.children match {
      case head :: tail =>
        // get an amount List[(Boolean, BigDecimal)] for each child (including disability and severe if it applies)
        period.children.foldLeft(BigDecimal(0.00))((acc, child) => {
          val basic = childOrYoungAdultBasicElementForPeriod(period, child)
          val disabled = childOrYoungAdultDisabilityElementForPeriod(period, child)
          val severeDisabled = childOrYoungAdultSevereDisabilityElementForPeriod(period, child)
          acc + basic + disabled + severeDisabled
        })
      case Nil => BigDecimal(0.00)
    }
  }

  def childOrYoungAdultBasicElementForPeriod(period: TCPeriod, child: TCChild): BigDecimal = {
    if (child.isQualifyingCTC) {
      val childElementMaximumAmount = if (child.childElements.youngAdult) {
        config(period).ctc.youngPersonElement
      }
      else {
        config(period).ctc.childElement
      }
      amountForDateRange(childElementMaximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def childOrYoungAdultDisabilityElementForPeriod(period: TCPeriod, child: TCChild): BigDecimal = {
    if (child.getsDisabilityElement) {
      val maximumAmount = config(period).ctc.disabledChildElement
      amountForDateRange(maximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def childOrYoungAdultSevereDisabilityElementForPeriod(period: TCPeriod, child: TCChild): BigDecimal = {
    if (child.getsSevereDisabilityElement) {
      val maximumAmount = config(period).ctc.severeDisabilityChildElement
      amountForDateRange(maximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def maxWorkElementForPeriod(period: TCPeriod): BigDecimal = {
    val basic = basicElementForPeriod(period)
    val hours30 = hours30ElementForPeriod(period)
    val loneParent = loneParentElementForPeriod(period)
    val secondAdult = secondAdultElementForPeriod(period)
    val houseHoldAmt = basic + hours30 + loneParent + secondAdult
    // get an amount List[(Boolean, BigDecimal)] for each claimant (including disability and severe if it applies)
    val claimantAmount = period.claimants.foldLeft(BigDecimal(0.00))((acc, claimant) => {
      val disabled = disabledWorkerElementForPeriod(period, claimant)
      val severeDisabled = severelyDisabledWorkerElementForPeriod(period, claimant)
      acc + disabled + severeDisabled
    })
    claimantAmount + houseHoldAmt
  }

  def basicElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.householdElements.basic) {
      val basicElement = config(period).wtc.basicElement
      amountForDateRange(basicElement, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def hours30ElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.householdElements.hours30) {
      val hours30Element = config(period).wtc.hours30Element
      amountForDateRange(hours30Element, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def loneParentElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.householdElements.loneParent) {
      val loneParentElementMaximumAmount = config(period).wtc.loneParentElement
      amountForDateRange(loneParentElementMaximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def secondAdultElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.householdElements.secondParent) {
      val coupleElementMaximumAmount = config(period).wtc.coupleElement
      amountForDateRange(coupleElementMaximumAmount, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def disabledWorkerElementForPeriod(period: TCPeriod, claimant: TCClaimant): BigDecimal = {
    if (claimant.getsDisabilityElement) {
      val disabledWorkerElement = config(period).wtc.disabledWorkerElement
      amountForDateRange(disabledWorkerElement, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def severelyDisabledWorkerElementForPeriod(period: TCPeriod, claimant: TCClaimant): BigDecimal = {
    if (claimant.getsSevereDisabilityElement) {
      val severeDisabilityWorkerElement = config(period).wtc.severeDisabilityWorkerElement
      amountForDateRange(severeDisabilityWorkerElement, Periods.Yearly, period.from, period.until)
    } else {
      BigDecimal(0.00)
    }
  }

  def maxChildcareElementForPeriod(period: TCPeriod): BigDecimal = {
    if (period.getChildCareForPeriod) {
      val totalCostPerWeek = getTotalChildcarePerWeek(period)
      // check threshold amounts
      val amountForPeriod = amountForDateRange(totalCostPerWeek, Periods.Weekly, period.from, period.until)

      val percent = config(period).wtc.eligibleCostCoveredPercent
      val percentOfActualAmountTapered = roundDownToTwoDigits(getPercentOfAmount(amountForPeriod, percent))
      val thresholdAmount = getChildcareThresholdPerWeek(period)
      val thresholdIntoAPeriod = amountForDateRange(thresholdAmount, Periods.Weekly, period.from, period.until)
      val percentOfThresholdAmountTapered = roundDownToTwoDigits(getPercentOfAmount(thresholdIntoAPeriod, percent))

      if (percentOfActualAmountTapered >= percentOfThresholdAmountTapered) {
        roundDownToTwoDigits(percentOfThresholdAmountTapered)
      } else {
        roundDownToTwoDigits(percentOfActualAmountTapered)
      }
    } else {
      BigDecimal(0.00)
    }
  }

  protected def getChildcareThresholdPerWeek(period: models.input.tc.TCPeriod): BigDecimal = {
    //check childcarecost > 0 and childcare element is true
    val childcareCosts = period.children.filter(child => (child.childcareCost > 0 && child.isQualifyingWTC)).length
    if (childcareCosts > 1) {
      config(period).wtc.maxChildcareMoreChildrenElement
    } else if (childcareCosts == 1) {
      config(period).wtc.maxChildcareOneChildElement
    } else {
      BigDecimal(0.00)
    }
  }

  private def getTotalChildcarePerWeek(period: TCPeriod) = {
    val amountPerWeek = period.children.foldLeft(BigDecimal(0.00)) {
      (acc, child) =>
        if (child.isQualifyingWTC) {
          // return cost per week
          val annual = amountToAnnualAmount(child.childcareCost, child.childcareCostPeriod)
          val amountPerWeek = amountToWeeklyAmount(annual, Periods.Yearly)
          acc + amountPerWeek
        } else {
          acc
        }
    }
    roundDownToTwoDigits(amountPerWeek)
  }
}

trait TCCalculatorTapering extends TCCalculatorHelpers {

  def isTaperingRequiredForElements(income: BigDecimal, threshold: BigDecimal): Boolean = {
    income > threshold
  }

  def getPercentOfAmount(amount: BigDecimal, percentage: Int): BigDecimal = {
    (amount / 100) * percentage
  }

  def taperFirstElement(
                         period: models.output.tc.Period,
                         inputPeriod: models.input.tc.TCPeriod,
                         income: BigDecimal,
                         wtcIncomeThreshold: BigDecimal
                       ): models.output.tc.Period = {
    val wtcWorkElementMaxAmount = period.elements.wtcWorkElement.maximumAmount
    val firstTaperThreshold = earningsAmountToTaperForPeriod(income, wtcIncomeThreshold, inputPeriod)
    val wtcWorkNetAmount = netAmountPerElementPerPeriod(firstTaperThreshold, wtcWorkElementMaxAmount)

    val elements = Elements(
      wtcWorkElement = Element(
        maximumAmount = wtcWorkElementMaxAmount,
        netAmount = wtcWorkNetAmount,
        taperAmount = {
          if (wtcWorkNetAmount.equals(BigDecimal(0.00))) {
            // cannot taper more than maximum amount for current element
            period.elements.wtcWorkElement.maximumAmount
          } else {
            firstTaperThreshold
          }
        }
      ),
      wtcChildcareElement = period.elements.wtcChildcareElement,
      ctcIndividualElement = period.elements.ctcIndividualElement,
      ctcFamilyElement = period.elements.ctcFamilyElement
    )

    buildTCPeriod(period, elements)
  }

  def taperSecondElement(
                          period: models.output.tc.Period,
                          inputPeriod: models.input.tc.TCPeriod,
                          income: BigDecimal,
                          wtcIncomeThreshold: BigDecimal
                        ): models.output.tc.Period = {
    val secondTaperAmount = earningsAmountToTaperForPeriod(income, wtcIncomeThreshold, inputPeriod)
    val secondTaperThreshold = secondTaperAmount - period.elements.wtcWorkElement.maximumAmount
    val wtcChildcareNetAmount = netAmountPerElementPerPeriod(secondTaperThreshold, period.elements.wtcChildcareElement.maximumAmount)

    val elements = Elements(
      wtcWorkElement = period.elements.wtcWorkElement,
      wtcChildcareElement = Element(
        maximumAmount = period.elements.wtcChildcareElement.maximumAmount,
        netAmount = {
          if (period.elements.wtcWorkElement.netAmount.equals(BigDecimal(0.00))) {
            wtcChildcareNetAmount
          } else {
            period.elements.wtcChildcareElement.maximumAmount
          }
        },
        taperAmount = {
          if (period.elements.wtcWorkElement.netAmount.equals(BigDecimal(0.00))) {
            if (wtcChildcareNetAmount.equals(BigDecimal(0.00))) {
              // cannot taper more than maximum amount for current element
              period.elements.wtcChildcareElement.maximumAmount
            } else {
              secondTaperThreshold
            }
          } else {
            BigDecimal(0.00)
          }
        }
      ),
      ctcIndividualElement = period.elements.ctcIndividualElement,
      ctcFamilyElement = period.elements.ctcFamilyElement
    )

    buildTCPeriod(period, elements)
  }

  def taperThirdElement(
                         period: models.output.tc.Period,
                         inputPeriod: models.input.tc.TCPeriod,
                         income: BigDecimal,
                         wtcIncomeThreshold: BigDecimal,
                         ctcIncomeThreshold: BigDecimal
                       ): (models.output.tc.Period, Boolean) = {

    val isNetAmountZero: Boolean = period.elements.wtcChildcareElement.netAmount.equals(BigDecimal(0.00))

    val taperingThresholdVal: Option[BigDecimal] = buildTaperingThresholdVal(
      isNetAmountZero,
      inputPeriod,
      period.elements.wtcWorkElement.maximumAmount + period.elements.wtcChildcareElement.maximumAmount,
      wtcIncomeThreshold,
      ctcIncomeThreshold
    )

    val elements = Elements(
      wtcWorkElement = period.elements.wtcWorkElement,
      wtcChildcareElement = period.elements.wtcChildcareElement,
      ctcIndividualElement = Element(
        maximumAmount = period.elements.ctcIndividualElement.maximumAmount,
        netAmount = getNetAmount(taperingThresholdVal, period.elements.ctcIndividualElement.maximumAmount, income, inputPeriod),
        taperAmount = getTaperAmount(taperingThresholdVal, period.elements.ctcIndividualElement.maximumAmount, income, inputPeriod)
      ),
      ctcFamilyElement = period.elements.ctcFamilyElement
    )
    (buildTCPeriod(period, elements), isNetAmountZero)
  }

  def buildTaperingThresholdVal(
                                 condition: Boolean,
                                 inputPeriod: models.input.tc.TCPeriod,
                                 income: BigDecimal,
                                 wtcIncomeThreshold: BigDecimal,
                                 ctcIncomeThreshold: BigDecimal
                               ): Option[BigDecimal] = {
    if (condition) {
      Some(getTaperingThreshold(
        inputPeriod,
        income,
        wtcIncomeThreshold,
        ctcIncomeThreshold
      ))
    } else {
      None
    }
  }

  def getTaperingThreshold(
                            inputPeriod: models.input.tc.TCPeriod,
                            incomeToTaper: BigDecimal,
                            wtcIncomeThreshold: BigDecimal,
                            ctcIncomeThreshold: BigDecimal
                          ): BigDecimal = {
    val taperRate = config(inputPeriod).thresholds.taperRatePercent
    val incomeToTaperVal = incomeToTaper / taperRate * 100 + wtcIncomeThreshold
    val roundedIncomeToTaperElementsNil = roundup(roundDownToTwoDigits(incomeToTaperVal))
    getHigherAmount(ctcIncomeThreshold, roundedIncomeToTaperElementsNil)
  }

  def getHigherAmount(amount1: BigDecimal, amount2: BigDecimal): BigDecimal = {
    if (amount1.>=(amount2)) {
      amount1
    } else {
      amount2
    }
  }

  def getNetAmount(taperingThresholdVal: Option[BigDecimal],
                   maximumAmount: BigDecimal,
                   income: BigDecimal,
                   inputPeriod: models.input.tc.TCPeriod): BigDecimal = {
    taperingThresholdVal match {
      case Some(taperingThreshold) if income.>(taperingThreshold) =>
        val taperAmount = earningsAmountToTaperForPeriod(income, taperingThreshold, inputPeriod)
        netAmountPerElementPerPeriod(taperAmount, maximumAmount)
      case _ => maximumAmount
    }
  }

  def getTaperAmount(taperingThresholdVal: Option[BigDecimal],
                     maximumAmount: BigDecimal, income: BigDecimal,
                     inputPeriod: models.input.tc.TCPeriod): BigDecimal = {
    taperingThresholdVal match {
      case Some(taperingThreshold) if income > taperingThreshold =>
        //further tapering required as income is too high
        val taperAmount = earningsAmountToTaperForPeriod(income, taperingThreshold, inputPeriod)
        val netAmount = netAmountPerElementPerPeriod(taperAmount, maximumAmount)
        if (netAmount.equals(BigDecimal(0.00))) {
          maximumAmount
        }
        else {
          taperAmount
        }
      //pass maximum amount if taperAmount needs to be used in next tapering
      case _ => BigDecimal(0.00)
    }
  }

  def netAmountPerElementPerPeriod(taperAmount: BigDecimal, maximumAmountPerElement: BigDecimal): BigDecimal = {
    if (taperAmount.>(maximumAmountPerElement)) {
      //WTC work element is nil and further taper is required
      BigDecimal(0.00)
    } else {
      //tapering stops send the difference in amount as WTC element
      maximumAmountPerElement - taperAmount
    }
  }

  //In order to maintain consistency across TC logic the amount is truncated to 3 digits after decimal point and rounded up to the nearest pence
  def earningsAmountToTaperForPeriod(income: BigDecimal, thresholdIncome: BigDecimal, period: TCPeriod): BigDecimal = {
    val taperRate = config(period).thresholds.taperRatePercent
    roundDownToTwoDigits((income - thresholdIncome) * (taperRate / BigDecimal(100.00)))
  }

  def buildTCPeriod(period: models.output.tc.Period, taperElements: Elements): models.output.tc.Period = {
    models.output.tc.Period(
      from = period.from,
      until = period.until,
      elements = taperElements
    )
  }

  def taperFourthElement(period: models.output.tc.Period,
                         inputPeriod: models.input.tc.TCPeriod,
                         income: BigDecimal,
                         wtcIncomeThreshold: BigDecimal,
                         ctcIncomeThreshold: BigDecimal,
                         continue: Boolean = false): models.output.tc.Period = {

    val taperingThresholdVal: Option[BigDecimal] = buildTaperingThresholdVal(
      continue,
      inputPeriod,
      period.elements.wtcWorkElement.maximumAmount + period.elements.wtcChildcareElement.maximumAmount + period.elements.ctcIndividualElement.maximumAmount,
      wtcIncomeThreshold,
      ctcIncomeThreshold
    )

    val elements = Elements(
      wtcWorkElement = period.elements.wtcWorkElement,
      wtcChildcareElement = period.elements.wtcChildcareElement,
      ctcIndividualElement = period.elements.ctcIndividualElement,
      ctcFamilyElement = Element(
        maximumAmount = period.elements.ctcFamilyElement.maximumAmount,
        netAmount = getNetAmount(taperingThresholdVal, period.elements.ctcFamilyElement.maximumAmount, income, inputPeriod),
        taperAmount = getTaperAmount(taperingThresholdVal, period.elements.ctcFamilyElement.maximumAmount, income, inputPeriod)
      )
    )
    buildTCPeriod(period, elements)
  }

  protected def getPeriodAmount(period: models.output.tc.Period, amount: BigDecimal = 0.00): models.output.tc.Period = {

    models.output.tc.Period(
      from = period.from,
      until = period.until,
      elements = Elements(
        wtcWorkElement = Element(
          maximumAmount = period.elements.wtcWorkElement.maximumAmount,
          netAmount = period.elements.wtcWorkElement.maximumAmount,
          taperAmount = BigDecimal(0.00)
        ),
        wtcChildcareElement = Element(
          maximumAmount = period.elements.wtcChildcareElement.maximumAmount,
          netAmount = period.elements.wtcChildcareElement.maximumAmount,
          taperAmount = BigDecimal(0.00)
        ),
        ctcIndividualElement = Element(
          maximumAmount = period.elements.ctcIndividualElement.maximumAmount,
          netAmount = period.elements.ctcIndividualElement.maximumAmount,
          taperAmount = BigDecimal(0.00)
        ),
        ctcFamilyElement = Element(
          maximumAmount = period.elements.ctcFamilyElement.maximumAmount,
          netAmount = period.elements.ctcFamilyElement.maximumAmount,
          taperAmount = BigDecimal(0.00)
        )
      ),
      periodNetAmount = amount,
      periodAdviceAmount = BigDecimal(0.00)
    )
  }
}

trait TCCalculatorHelpers extends CCCalculatorHelper {
  val tcConfig: TCConfig

  def config(period: TCPeriod): TCTaxYearConfig = tcConfig.getConfig(period.from)

  def amountForDateRange(
                          cost: BigDecimal,
                          period: Periods.Period,
                          fromDate: LocalDate,
                          toDate: LocalDate
                        ): BigDecimal = {
    if (fromDate.isBefore(toDate)) {
      //determines if the tax year falls in a leap year and uses 366 days instead of 365 in calculation
      val taxYearDates = tcConfig.getCurrentTaxYearDateRange(fromDate)
      val numberOfDaysInTaxYear = daysBetween(taxYearDates._1, taxYearDates._2)
      //daily amount currently is not rounded
      val dailyAmount = amountFromPeriodToDaily(cost, period, numberOfDaysInTaxYear)
      val dailyAmountRounded = roundDownToTwoDigits(dailyAmount)
      val numberOfDays = daysBetween(fromDate, toDate)
      dailyAmountRounded * numberOfDays
    }
    else {
      BigDecimal(0.00)
    }
  }

  def getTotalMaximumAmountPerPeriod(period: models.output.tc.Period): BigDecimal = {
    period.elements.wtcWorkElement.maximumAmount +
      period.elements.wtcChildcareElement.maximumAmount +
      period.elements.ctcIndividualElement.maximumAmount +
      period.elements.ctcFamilyElement.maximumAmount
  }

}

class TCCalculator @Inject()(val tcConfig: TCConfig) extends TCCalculatorElements with TCCalculatorHelpers {

  import scala.concurrent.ExecutionContext.Implicits.global

  def award(request: TCCalculatorInput): Future[models.output.tc.TCCalculatorOutput] = {
    Future {
      val calculatedTaxYears = getCalculatedTaxYears(request)
      createTCCalculation(calculatedTaxYears, annualIncome(calculatedTaxYears))
    }
  }


  protected def calculateHouseholdIncome(from: LocalDate, previousIncome: TCIncome, currentIncome: TCIncome): BigDecimal = {

    val tcConf = tcConfig.getConfig(from)
    def getAmount(income: TCIncome): BigDecimal = {
      val employment: BigDecimal = income.employment.getOrElse(List()).foldLeft(BigDecimal(0))(_ + _)
      val pension: BigDecimal = income.pension.getOrElse(List()).foldLeft(BigDecimal(0))(_ + _)
      val benefits: BigDecimal = income.benefits.getOrElse(List()).foldLeft(BigDecimal(0))(_ + _)
      val statutory: BigDecimal = income.statutory.getOrElse(List()).foldLeft(BigDecimal(0))((acc, stat) => acc + stat.weeks * stat.amount)
      val other: BigDecimal = income.other.getOrElse(List()).foldLeft(BigDecimal(0))(_ + _)

      val otherAdjustment: BigDecimal = if (other > tcConf.otherIncomeAdjustment) {
        tcConf.otherIncomeAdjustment
      } else {
        other
      }

      // benefits and pension are asked monthly so need to multiply it by 12
      (benefits - pension) * tcConfig.monthsInTaxYear + employment + other - otherAdjustment - statutory
    }

    val previousAmount: BigDecimal = getAmount(previousIncome)
    val currentAmount: BigDecimal = getAmount(currentIncome)

    val difference: BigDecimal = Math.abs((currentAmount - previousAmount).toDouble)
    (previousAmount, currentAmount) match {
      case (previous, current) if previous > current && difference > tcConf.currentIncomeFallDifferenceAmount =>
        previous + tcConf.currentIncomeFallDifferenceAmount - difference
      case (previous, current) if previous < current && difference > tcConf.currentIncomeRiseDifferenceAmount =>
        previous + difference - tcConf.currentIncomeRiseDifferenceAmount
      case _ => previousAmount
    }

  }

  def getCalculatedTaxYears(inputTCEligibility: TCCalculatorInput): List[TaxYear] = {

    for (taxYear <- inputTCEligibility.taxYears) yield {
      val householdIncome = calculateHouseholdIncome(taxYear.from, taxYear.previousHouseholdIncome, taxYear.currentHouseholdIncome)
      val calculatedPeriods = getCalculatedPeriods(taxYear, householdIncome)
      val annualAward = calculatedPeriods.foldLeft(BigDecimal(0.00))((acc, period) => acc + period.periodNetAmount)

      models.output.tc.TaxYear(
        from = taxYear.from,
        until = taxYear.until,
        taxYearAwardAmount = annualAward,
        periods = calculatedPeriods
      )
    }
  }

  def getCalculatedPeriods(
                            taxYear: models.input.tc.TCTaxYear,
                            previousHouseholdIncome: BigDecimal
                          ): List[models.output.tc.Period] = {
    for (period <- taxYear.periods) yield {
      val periodLength = Periods.Yearly
      // get all the elements for the period (pro-rota to the number of days) for each household composition
      val income = amountForDateRange(previousHouseholdIncome, periodLength, period.from, period.until)

      val wtcThresholdConfig = config(period).thresholds.wtcIncomeThreshold
      val wtcIncomeThreshold = amountForDateRange(wtcThresholdConfig, periodLength, period.from, period.until)
      val ctcThresholdConfig = config(period).thresholds.ctcIncomeThreshold
      val ctcIncomeThreshold = amountForDateRange(ctcThresholdConfig, periodLength, period.from, period.until)
      // return an award period which contains all the elements and their amounts they can claim for that period
      val maximumAmounts = generateMaximumAmountsForPeriod(period)
      //here we get the model updated with net due and taper and advice amounts
      generateRequiredAmountsPerPeriod(
        maximumAmounts,
        period,
        income,
        wtcIncomeThreshold,
        ctcIncomeThreshold
      )
    }
  }

  def generateRequiredAmountsPerPeriod(
                                        period: models.output.tc.Period,
                                        inputPeriod: models.input.tc.TCPeriod,
                                        income: BigDecimal,
                                        wtcIncomeThreshold: BigDecimal,
                                        ctcIncomeThreshold: BigDecimal): models.output.tc.Period = {
    val totalMaximumAmount = getTotalMaximumAmountPerPeriod(period)
    if (isTaperingRequiredForElements(income, wtcIncomeThreshold) && !inputPeriod.atLeastOneClaimantIsClaimingSocialSecurityBenefit) {
      //call taper 1, taper 2, taper 3, taper 4
      val taperedFirstElement = taperFirstElement(period, inputPeriod, income, wtcIncomeThreshold)
      val taperedSecondElement = taperSecondElement(taperedFirstElement, inputPeriod, income, wtcIncomeThreshold)
      val taperedThirdElement = taperThirdElement(taperedSecondElement, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      val taperedFourthElement = taperFourthElement(
        taperedThirdElement._1, inputPeriod,
        income,
        wtcIncomeThreshold,
        ctcIncomeThreshold,
        taperedThirdElement._2
      )

      models.output.tc.Period(
        from = period.from,
        until = period.until,
        elements = Elements(
          wtcWorkElement = taperedFourthElement.elements.wtcWorkElement,
          wtcChildcareElement = taperedFourthElement.elements.wtcChildcareElement,
          ctcIndividualElement = taperedFourthElement.elements.ctcIndividualElement,
          ctcFamilyElement = taperedFourthElement.elements.ctcFamilyElement
        ),
        periodNetAmount = {
          taperedFourthElement.elements.wtcWorkElement.netAmount +
            taperedFourthElement.elements.wtcChildcareElement.netAmount +
            taperedFourthElement.elements.ctcIndividualElement.netAmount +
            taperedFourthElement.elements.ctcFamilyElement.netAmount
        }
      )
    } else {
      getPeriodAmount(period, totalMaximumAmount)
    }
  }

  private def createTCCalculation(calculatedTaxYears: List[TaxYear], annualIncome: BigDecimal) = {

    TCCalculatorOutput(
      from = calculatedTaxYears.head.from,
      until = {
        if (calculatedTaxYears.length > 1) {
          calculatedTaxYears.tail.head.until
        } else {
          calculatedTaxYears.head.until
        }
      },
      houseHoldAdviceAmount = BigDecimal(0.00),
      totalAwardAmount = annualIncome,
      taxYears = calculatedTaxYears
    )
  }

  private def annualIncome(taxYears: List[TaxYear]): BigDecimal = {
    taxYears.foldLeft(BigDecimal(0.00))((acc, taxYear) => {
      acc + taxYear.taxYearAwardAmount
    })
  }

}
