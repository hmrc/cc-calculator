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

import models.input.tc._
import models.output.tc.{Element, Elements, TCCalculation, TaxYear}
import org.joda.time.{Days, LocalDate}
import utils.{Periods, TCConfig}
import scala.concurrent.Future
import scala.math.BigDecimal.RoundingMode

object TCCalculator extends TCCalculator

trait TCCalculator {

  val calculator = new TCCalculatorService

  trait TCCalculatorElements {
    this: TCCalculatorService =>

    def generateMaximumAmountsForPeriod(period: Period): models.output.tc.Period = {
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

    def maxFamilyElementForPeriod(period: Period): BigDecimal = {
      if (period.householdElements.family) {
        val familyElementMaximumAmount = period.config.ctc.familyElement
        (amountForDateRange(familyElementMaximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def maxChildElementForPeriod(period: Period): BigDecimal = {
      period.children match {
        case head :: tail =>
          // get an amount List[(Boolean, BigDecimal)] for each child (including disability and severe if it applies)
          period.children.foldLeft(BigDecimal(0.00))((acc, child) => {
            val basic = childOrYoungAdultBasicElementForPeriod(period, child)
            val disabled = childOrYoungAdultDisabilityElementForPeriod(period, child)
            val severeDisabled = childOrYoungAdultSevereDisabilityElementForPeriod(period, child)
            (acc + basic + disabled + severeDisabled)
          })
        case Nil => BigDecimal(0.00)
      }
    }

    def childOrYoungAdultBasicElementForPeriod(period: Period, child: Child): BigDecimal = {
      if (child.isQualifyingCTC) {
        val childElementMaximumAmount = if (child.childElements.youngAdult) {
          period.config.ctc.youngPersonElement
        }
        else {
          period.config.ctc.childElement
        }
        (amountForDateRange(childElementMaximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def childOrYoungAdultDisabilityElementForPeriod(period: Period, child: Child): BigDecimal = {
      if (child.getsDisabilityElement) {
        val maximumAmount = period.config.ctc.disabledChildElement
        (amountForDateRange(maximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def childOrYoungAdultSevereDisabilityElementForPeriod(period: Period, child: Child): BigDecimal = {
      if (child.getsSevereDisabilityElement) {
        val maximumAmount = period.config.ctc.severeDisabilityChildElement
        (amountForDateRange(maximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def maxWorkElementForPeriod(period: Period): BigDecimal = {
      val basic = basicElementForPeriod(period)
      val hours30 = hours30ElementForPeriod(period)
      val loneParent = loneParentElementForPeriod(period)
      val secondAdult = secondAdultElementForPeriod(period)
      val houseHoldAmt = basic + hours30 + loneParent + secondAdult
      // get an amount List[(Boolean, BigDecimal)] for each claimant (including disability and severe if it applies)
      val claimantAmount = period.claimants.foldLeft(BigDecimal(0.00))((acc, claimant) => {
        val disabled = disabledWorkerElementForPeriod(period, claimant)
        val severeDisabled = severelyDisabledWorkerElementForPeriod(period, claimant)
        (acc + disabled + severeDisabled)
      })
      (claimantAmount + houseHoldAmt)
    }

    def basicElementForPeriod(period: Period): BigDecimal = {
      if (period.householdElements.basic) {
        val basicElement = period.config.wtc.basicElement
        (amountForDateRange(basicElement, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def hours30ElementForPeriod(period: Period): BigDecimal = {
      if (period.householdElements.hours30) {
        val hours30Element = period.config.wtc.hours30Element
        (amountForDateRange(hours30Element, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def loneParentElementForPeriod(period: Period): BigDecimal = {
      if (period.householdElements.loneParent) {
        val loneParentElementMaximumAmount = period.config.wtc.loneParentElement
        (amountForDateRange(loneParentElementMaximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def secondAdultElementForPeriod(period: Period): BigDecimal = {
      if (period.householdElements.secondParent) {
        val coupleElementMaximumAmount = period.config.wtc.coupleElement
        (amountForDateRange(coupleElementMaximumAmount, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def disabledWorkerElementForPeriod(period: Period, claimant: Claimant): BigDecimal = {
      if (claimant.getsDisabilityElement) {
        val disabledWorkerElement = period.config.wtc.disabledWorkerElement
        (amountForDateRange(disabledWorkerElement, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def severelyDisabledWorkerElementForPeriod(period: Period, claimant: Claimant): BigDecimal = {
      if (claimant.getsSevereDisabilityElement) {
        val severeDisabilityWorkerElement = period.config.wtc.severeDisabilityWorkerElement
        (amountForDateRange(severeDisabilityWorkerElement, Periods.Yearly, period.from, period.until))
      } else {
        BigDecimal(0.00)
      }
    }

    def maxChildcareElementForPeriod(period: Period): BigDecimal = {
      if (period.getChildCareForPeriod) {
        val totalCostPerWeek = getTotalChildcarePerWeek(period)
        // check threshold amounts
        val amountForPeriod = amountForDateRange(totalCostPerWeek, Periods.Weekly, period.from, period.until)

        val percent = period.config.wtc.eligibleCostCoveredPercent
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

    protected def getChildcareThresholdPerWeek(period: models.input.tc.Period): BigDecimal = {
      //check childcarecost > 0 and childcare element is true
      val childcareCosts = period.children.filter(child => (child.childcareCost > 0 && child.childElements.childcare)).length
      if (childcareCosts > 1) {
        (period.config.wtc.maxChildcareMoreChildrenElement)
      } else if (childcareCosts == 1) {
        (period.config.wtc.maxChildcareOneChildElement)
      } else {
        BigDecimal(0.00)
      }
    }

    private def getTotalChildcarePerWeek(period: Period) = {
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

  trait TCCalculatorTapering {
    this: TCCalculatorService =>

    def isTaperingRequiredForElements(income: BigDecimal, threshold: BigDecimal): Boolean = {
      income > threshold
    }

    def getPercentOfAmount(amount: BigDecimal, percentage: Int): BigDecimal = {
      (amount / 100) * percentage
    }

    def wtcIncomeThresholdForPeriod(period: Period): BigDecimal = {
      val thresholdConfig = period.config.thresholds.wtcIncomeThreshold
      amountForDateRange(thresholdConfig, Periods.Yearly, period.from, period.until)
    }

    def ctcIncomeThresholdForPeriod(period: Period): BigDecimal = {
      val thresholdConfig = period.config.thresholds.ctcIncomeThreshold
      amountForDateRange(thresholdConfig, Periods.Yearly, period.from, period.until)
    }

    def incomeForPeriod(previousHouseHoldIncome: BigDecimal, period: Period): BigDecimal = {
      amountForDateRange(previousHouseHoldIncome, Periods.Yearly, period.from, period.until)
    }

    def taperFirstElement(
                           period: models.output.tc.Period,
                           inputPeriod: models.input.tc.Period,
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
                            inputPeriod: models.input.tc.Period,
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
                           inputPeriod: models.input.tc.Period,
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
                                   inputPeriod: models.input.tc.Period,
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
                              inputPeriod: models.input.tc.Period,
                              incomeToTaper: BigDecimal,
                              wtcIncomeThreshold: BigDecimal,
                              ctcIncomeThreshold: BigDecimal
                            ): BigDecimal = {
      val taperRate = inputPeriod.config.thresholds.taperRatePercent
      val incomeToTaperVal = incomeToTaper / taperRate * 100 + wtcIncomeThreshold
      val roundedIncomeToTaperElementsNil = round(roundDownToTwoDigits(incomeToTaperVal))
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
                     inputPeriod: models.input.tc.Period): BigDecimal = {
      taperingThresholdVal match {
        case Some(taperingThreshold) if income.>(taperingThreshold) => {
          val taperAmount = earningsAmountToTaperForPeriod(income, taperingThreshold, inputPeriod)
          netAmountPerElementPerPeriod(taperAmount, maximumAmount)
        }
        case _ => maximumAmount
      }
    }

    def getTaperAmount(taperingThresholdVal: Option[BigDecimal],
                       maximumAmount: BigDecimal, income: BigDecimal,
                       inputPeriod: models.input.tc.Period): BigDecimal = {
      taperingThresholdVal match {
        case Some(taperingThreshold) if (income.>(taperingThreshold)) => {
          //further tapering required as income is too high
          val taperAmount = earningsAmountToTaperForPeriod(income, taperingThreshold, inputPeriod)
          val netAmount = netAmountPerElementPerPeriod(taperAmount, maximumAmount)
          if (netAmount.equals(BigDecimal(0.00))) {
            maximumAmount
          }
          else {
            taperAmount
          }
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
    def earningsAmountToTaperForPeriod(income: BigDecimal, thresholdIncome: BigDecimal, period: Period): BigDecimal = {
      val taperRate = period.config.thresholds.taperRatePercent
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
                           inputPeriod: models.input.tc.Period,
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

    protected def getPeriodAmount(period: models.output.tc.Period, amount: BigDecimal = 0.00, fullCalculationRequired: Boolean): models.output.tc.Period = {
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
        periodNetAmount = {
          if (fullCalculationRequired) amount else BigDecimal(0.00)
        },
        periodAdviceAmount = {
          if (fullCalculationRequired) BigDecimal(0.00) else amount
        }
      )

    }
  }

  trait TCCalculatorHelpers {
    this: TCCalculatorService =>

    /**
      * Unformatted:   5.46102
      * Formatted:     5.47
      *
      * For TC uses a different rounding rules, where rounding always increments the digit prior to a non-zero value
      */
    def round(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.UP)

    /**
      * Unformatted:   5.001
      * Formatted:     6.00
      *
      * For TC uses a different rounding rules, where rounding always increments the digit prior to a non-zero value
      */
    def roundToPound(value: BigDecimal): BigDecimal = value.setScale(0, RoundingMode.UP)

    def roundDownToTwoDigits(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_EVEN)

    def daysBetween(fromDate: LocalDate, toDate: LocalDate): Int = {
      Days.daysBetween(fromDate, toDate).getDays
    }

    def amountForDateRange(
                            cost: BigDecimal,
                            period: Periods.Period,
                            fromDate: LocalDate,
                            toDate: LocalDate
                          ): BigDecimal = {
      if (fromDate.isBefore(toDate)) {
        //determines if the tax year falls in a leap year and uses 366 days instead of 365 in calculation
        val taxYearDates = TCConfig.getCurrentTaxYearDateRange(fromDate)
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

    def amountToAnnualAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      fromPeriod match {
        case Periods.Weekly => cost * 52
        case Periods.Fortnightly => cost * 26
        case Periods.Monthly => cost * 12
        case Periods.Quarterly => cost * 4
        case Periods.Yearly => cost
        case _ => 0.00 //error
      }
    }

    def amountToWeeklyAmount(cost: BigDecimal, fromPeriod: Periods.Period): BigDecimal = {
      fromPeriod match {
        case Periods.Weekly => cost
        case Periods.Fortnightly => cost / 2
        case Periods.Monthly => (cost * 12) / 52
        case Periods.Quarterly => (cost * 4) / 52
        case Periods.Yearly => cost / 52
        case _ => 0.00 //error
      }
    }

    def amountFromPeriodToDaily(cost: BigDecimal, fromPeriod: Periods.Period, daysInTheYear: Int): BigDecimal = {
      val amount: BigDecimal = fromPeriod match {
        case Periods.Weekly => (cost * 52) / daysInTheYear
        case Periods.Fortnightly => (cost * 26) / daysInTheYear
        case Periods.Monthly => (cost * 12) / daysInTheYear
        case Periods.Quarterly => (cost * 4) / daysInTheYear
        case Periods.Yearly => cost / daysInTheYear
        case _ => 0.00 //error
      }
      amount
    }

    def getTotalMaximumAmountPerPeriod(period: models.output.tc.Period): BigDecimal = {
      period.elements.wtcWorkElement.maximumAmount +
        period.elements.wtcChildcareElement.maximumAmount +
        period.elements.ctcIndividualElement.maximumAmount +
        period.elements.ctcFamilyElement.maximumAmount
    }

  }

  class TCCalculatorService extends TCCalculatorElements with TCCalculatorTapering with
    TCCalculatorHelpers {

    import scala.concurrent.ExecutionContext.Implicits.global

    def award(request: TCEligibility): Future[models.output.tc.TCCalculation] = {
      awardPeriod(request, false)
    }

    def incomeAdvice(request: TCEligibility): Future[models.output.tc.TCCalculation] = {
      awardPeriod(request, true)
    }

    private def awardPeriod(request: TCEligibility, incomeAdvice: Boolean = false) = {
      Future {
        val calculatedTaxYears = getCalculatedTaxYears(request, incomeAdvice)
        createTCCalculation(calculatedTaxYears, annualIncome(calculatedTaxYears, incomeAdvice), incomeAdvice)
      }
    }

    def getCalculatedTaxYears(inputTCEligibility: TCEligibility, incomeAdviceCalculation: Boolean = false): List[TaxYear] = {
      for (taxYear <- inputTCEligibility.taxYears) yield {
        if (incomeAdviceCalculation) {
          //calculating the income advice
          val calculatedPeriods = getCalculatedPeriods(taxYear, taxYear.houseHoldIncome, fullCalculationRequired = false)
          val adviceAmount = calculatedPeriods.foldLeft(BigDecimal(0.00))((acc, period) => acc + period.periodAdviceAmount)

          models.output.tc.TaxYear(
            from = taxYear.from,
            until = taxYear.until,
            taxYearAdviceAmount = adviceAmount,
            periods = calculatedPeriods
          )
        } else {
          // full calculation including tapering
          val calculatedPeriods = getCalculatedPeriods(taxYear, taxYear.houseHoldIncome, fullCalculationRequired = true)
          val annualAward = calculatedPeriods.foldLeft(BigDecimal(0.00))((acc, period) => acc + period.periodNetAmount)

          models.output.tc.TaxYear(
            from = taxYear.from,
            until = taxYear.until,
            taxYearAwardAmount = annualAward,
            periods = calculatedPeriods
          )
        }
      }
    }

    def getCalculatedPeriods(
                              taxYear: models.input.tc.TaxYear,
                              previousHouseholdIncome: BigDecimal,
                              fullCalculationRequired: Boolean = true
                            ): List[models.output.tc.Period] = {
      for (period <- taxYear.periods) yield {
        // get all the elements for the period (pro-rota to the number of days) for each household composition
        val income = incomeForPeriod(previousHouseholdIncome, period)
        val wtcIncomeThreshold = wtcIncomeThresholdForPeriod(period)
        val ctcIncomeThreshold = ctcIncomeThresholdForPeriod(period)
        // return an award period which contains all the elements and their amounts they can claim for that period
        val maximumAmounts = generateMaximumAmountsForPeriod(period)
        //here we get the model updated with net due and taper and advice amounts
        generateRequiredAmountsPerPeriod(
          maximumAmounts,
          period,
          income,
          wtcIncomeThreshold,
          ctcIncomeThreshold,
          fullCalculationRequired
        )
      }
    }

    def generateRequiredAmountsPerPeriod(
                                          period: models.output.tc.Period,
                                          inputPeriod: models.input.tc.Period,
                                          income: BigDecimal,
                                          wtcIncomeThreshold: BigDecimal,
                                          ctcIncomeThreshold: BigDecimal,
                                          fullCalculationRequired: Boolean = true): models.output.tc.Period = {
      val totalMaximumAmount = getTotalMaximumAmountPerPeriod(period)
      if (fullCalculationRequired) {
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
          //When no tapering is required
          getPeriodAmount(period, totalMaximumAmount, fullCalculationRequired)
        }
      }
      else {
        // if calculating household advice
        val adviceAmount = getAdviceCalculationRounded(totalMaximumAmount, wtcIncomeThreshold, inputPeriod)
        getPeriodAmount(period, adviceAmount, fullCalculationRequired)
      }

    }

    //reverse taper rate is truncated to 3 decimal places
    //currently using just WTC threshold to calculate advice
    def getAdviceCalculationRounded(totalAmount: BigDecimal, wtcIncomeThreshold: BigDecimal, period: models.input.tc.Period): BigDecimal = {
      val taperPercentage = period.config.thresholds.taperRatePercent
      val reverseTaperRate = BigDecimal(100.00) / BigDecimal(taperPercentage)
      val reverseTaperRateRounded = roundDownToTwoDigits(reverseTaperRate)
      val adviceAmount = (reverseTaperRateRounded * totalAmount) + wtcIncomeThreshold
      adviceAmount
    }

    private def createTCCalculation(calculatedTaxYears: List[TaxYear], annualIncome: BigDecimal, incomeAdvice: Boolean = false) = {
      TCCalculation(
        from = calculatedTaxYears.head.from,
        until = {
          if (calculatedTaxYears.length > 1) {
            calculatedTaxYears.tail.head.until
          } else {
            calculatedTaxYears.head.until
          }
        },
        houseHoldAdviceAmount = if (incomeAdvice) {
          annualIncome
        } else {
          BigDecimal(0.00)
        },
        totalAwardAmount = if (incomeAdvice) {
          BigDecimal(0.00)
        } else {
          annualIncome
        },
        taxYears = calculatedTaxYears
      )
    }

    private def annualIncome(taxYears: List[TaxYear], incomeAdvice: Boolean = false): BigDecimal = {
      taxYears.foldLeft(BigDecimal(0.00))((acc, taxYear) => {
        if (incomeAdvice) {
          acc + taxYear.taxYearAdviceAmount
        } else {
          acc + taxYear.taxYearAwardAmount
        }
      })
    }

  }

}
