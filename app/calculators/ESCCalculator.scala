/*
 * Copyright 2018 HM Revenue & Customs
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

import models.input.esc._
import models.output.esc.{ESCCalculatorOutput, ESCSavings}
import models.utility.{CalculationNIBands, CalculationTaxBands}
import org.joda.time.LocalDate
import play.api.Logger
import utils.{ESCConfig, ESCTaxYearConfig, MessagesObject, Periods}

import scala.concurrent.Future

trait ESCCalculatorHelpers extends ESCConfig with CCCalculatorHelper with MessagesObject {

  private def toInt(string : String): Option[Int] = {
    try {
      Some(string.toInt)
    } catch {
      case e : Exception =>
        None
    }
  }

  def validateCode(code: String): Boolean =  code.equals("BR") || code.equals("D0") || code.equals("D1") || code.equals("NT") ||
    code.equals("0T") || code.startsWith("K")
  def validateCodeBasedOnEndsWith(code: String): Boolean =  code.endsWith("L") || code.endsWith("M") || code.endsWith("N") ||
    code.endsWith("T") || code.endsWith("Y") || code.endsWith("S")
  def validateEmergencyCode(code: String): Boolean = code.endsWith("W1") || code.endsWith("M1") || code.endsWith("X")

  private def extractEmergencyCode(code: String): (BigDecimal, String) = {
    val extractNumber = if(code.endsWith("1")) {
      toInt(code.substring(0, code.length - 2))
    } else {
      toInt(code.substring(0, code.length - 1))
    }

    extractNumber match {
      case Some(number) => (number * 10, code)
      case None =>
        Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.extractEmergencyCode - Exception case None")
        throw new NoSuchElementException(messages("cc.scheme.config.invalid.tax.code"))
    }
  }


  def validateTaxCode(period: ESCPeriod, income: ESCTotalIncome): (BigDecimal, String) = {
    income.taxCode.toUpperCase.trim match {
      case code if validateCode(code) => (BigDecimal(0.00), code)
      case code if validateCodeBasedOnEndsWith(code) =>
        toInt(code.substring(0, code.length - 1)) match {
          case Some(number) => (number * 10, code)
          case None =>
            Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.validateTaxCode - Exception case None")
            throw new NoSuchElementException(messages("cc.scheme.config.invalid.tax.code"))
        }
      case code if validateEmergencyCode(code) =>
        extractEmergencyCode(code.toUpperCase.trim)
      case _ =>
        Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.validateTaxCode - Exception case others")
        throw new NoSuchElementException(messages("cc.scheme.config.invalid.tax.code"))
    }
  }



  def getTaxCode(period: ESCPeriod, income: ESCTotalIncome, config: ESCTaxYearConfig) : String = {
    income.taxCode.trim match {
      case code if code.trim.isEmpty => config.defaultTaxCode
      case _ => validateTaxCode(period, income)._2
    }
  }

  def getAnnualRelevantEarnings(income: ESCTotalIncome, period: ESCPeriod, config: ESCTaxYearConfig): (BigDecimal, BigDecimal) = {
    val higherRateCeiling = config.taxHigherBandUpperLimit
    val personalAllowance = config.defaultPersonalAllowance
    val niLimit = config.niLimit
    income.gross match {
      case gross if gross <= niLimit => (BigDecimal(0.00), BigDecimal(0.00))
      case gross if gross <= personalAllowance => (BigDecimal(0.00), income.taxablePay - niLimit)
      case gross if gross <= higherRateCeiling => (income.taxablePay - personalAllowance, personalAllowance - niLimit)
      case gross if gross > higherRateCeiling => (income.taxablePay, BigDecimal(0.00))
    }
  }

  //Scottish tax changes
  private def determineMaximumIncomeReliefPost2011BasedOnRelevantEarnings(relevantEarnings: BigDecimal,
                                                                          calcPeriod: Periods.Period = Periods.Monthly,
                                                                          config: ESCTaxYearConfig) = {
    val higherRateCeiling : BigDecimal = config.taxHigherBandUpperLimit
    val basicRateCeiling : BigDecimal = config.taxBasicBandCapacity

    val isScottishESCTaxYearConfig = config.taxStarterRate > 0
    val niThreshold = config.basicNiThresholdUk

    //Need refactoring
    if(isScottishESCTaxYearConfig){
        relevantEarnings match {
          case amount if amount <= niThreshold => //20% band, if you earn less than PA, you could still buy only that amount of vouchers
            monthlyAmountToPeriod(config.post2011MaxExemptionMonthlyBasic, calcPeriod)
          case amount if amount > niThreshold && amount <= higherRateCeiling => //40% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyHigher, calcPeriod)
          case amount if amount > higherRateCeiling => //45% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyAdditional, calcPeriod)
        }
    } else {
        relevantEarnings match {
          case amount if amount <= basicRateCeiling => //20% band, if you earn less than PA, you could still buy only that amount of vouchers
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyBasic, calcPeriod)
          case amount if amount > basicRateCeiling && amount <= higherRateCeiling => //40% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyHigher, calcPeriod)
          case amount if amount > higherRateCeiling => //45% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyAdditional, calcPeriod)
        }
    }

  }

  private def determineMaximumIncomeReliefPre2011(relevantEarnings: BigDecimal,
                                                  calcPeriod: Periods.Period = Periods.Monthly,
                                                  taxCode: String) = {
    relevantEarnings match {
      case amount if amount <= 0 => BigDecimal(0.00)
      case amount if amount > 0 =>
        taxCode.toUpperCase match {
          case code if zeroTaxCode(code) => BigDecimal(0.00)
          case _ => monthlyAmountToPeriod(pre2011MaxExemptionMonthly, calcPeriod)
        }
    }
  }

  private def zeroTaxCode(code: String): Boolean = code.contains("NT") || code.contains("0T") || code.startsWith("K")

  def determineMaximumIncomeRelief(
                                    pre2011: Boolean,
                                    relevantEarnings: BigDecimal,
                                    calcPeriod: Periods.Period = Periods.Monthly,
                                    taxCode: String,
                                    config: ESCTaxYearConfig
                                  ): BigDecimal = {
    pre2011 match {
      case true => determineMaximumIncomeReliefPre2011(relevantEarnings, calcPeriod, taxCode)
      case false =>
        taxCode.toUpperCase match {
          case "BR" => //20% band
            monthlyAmountToPeriod(config.post2011MaxExemptionMonthlyBasic, calcPeriod)
          case "D0" => //40% band
            monthlyAmountToPeriod(config.post2011MaxExemptionMonthlyHigher, calcPeriod)
          case "D1" => //45% band
            monthlyAmountToPeriod(config.post2011MaxExemptionMonthlyAdditional, calcPeriod)
          case code if zeroTaxCode(code) => BigDecimal(0.00)
          case _ => determineMaximumIncomeReliefPost2011BasedOnRelevantEarnings(relevantEarnings, calcPeriod, config)
        }
    }
  }

  def determineActualIncomeRelief(voucherAmount : BigDecimal, maxReliefAmount : BigDecimal) : BigDecimal = {
    voucherAmount match {
      case amount if amount <= maxReliefAmount => amount
      case _ => maxReliefAmount
    }
  }

  def subtractActualReliefFromIncome(income : BigDecimal, maximumRelief : BigDecimal, calcPeriod : Periods.Period) : BigDecimal = {
    val incomePerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(income, calcPeriod))

    incomePerPeriod match {
      case amount if amount <= maximumRelief => BigDecimal(0.00)
      case amount => amount - maximumRelief
    }
  }

  def determineTotalSavings(preSalarySacrificeAmount : BigDecimal, postSalarySacrificeAmount : BigDecimal) : BigDecimal = {
    preSalarySacrificeAmount match {
      case amount if amount <= postSalarySacrificeAmount => BigDecimal(0.00)
      case amount => amount - postSalarySacrificeAmount
    }
  }

  protected def populateClaimantModel(
                                       qualifying: Boolean,
                                       eligibleMonths: Int,
                                       isPartner: Boolean,
                                       taxablePay: BigDecimal,
                                       grossPay: BigDecimal,
                                       taxCode: String,
                                       niCategory: String,
                                       vouchers: Boolean,
                                       escAmount: BigDecimal,
                                       escAmountPeriod: Periods.Period,
                                       escStartDate: LocalDate,
                                       totalSaving: BigDecimal,
                                       taxSaving: BigDecimal,
                                       niSaving: BigDecimal,
                                       taxPaidPreSacrifice: BigDecimal,
                                       niPaidPreSacrifice: BigDecimal,
                                       taxPaidPostSacrifice: BigDecimal,
                                       niPaidPostSacrifice: BigDecimal
                                     ): models.output.esc.ESCClaimant = {
    models.output.esc.ESCClaimant(
      qualifying = qualifying,
      eligibleMonthsInTaxYear = eligibleMonths,
      isPartner = isPartner,
      income = models.output.esc.ESCIncome(taxablePay = taxablePay, gross = grossPay, taxCode = taxCode, niCategory = niCategory),
      vouchers = vouchers,
      escAmount = escAmount,
      escAmountPeriod = escAmountPeriod,
      escStartDate = escStartDate,
      savings = models.output.esc.ESCSavings(totalSaving = totalSaving, taxSaving = taxSaving, niSaving = niSaving),
      taxAndNIBeforeSacrifice = models.output.esc.ESCTaxAndNi(taxPaid = taxPaidPreSacrifice, niPaid = niPaidPreSacrifice),
      taxAndNIAfterSacrifice = models.output.esc.ESCTaxAndNi(taxPaid = taxPaidPostSacrifice, niPaid = niPaidPostSacrifice)
    )
  }
}

//Scottish tax changes
trait ESCCalculatorTax extends ESCCalculatorHelpers {

  private def allocateAmountToTaxBandsBasedOnTaxablePay(taxablePay: BigDecimal,
                                                        personalAllowancePerPeriod: BigDecimal,
                                                        calcPeriod: Periods.Period,
                                                        config: ESCTaxYearConfig): CalculationTaxBands =

    // Check for the Scottish ESCTaxYearConfig
    if (config.taxStarterRate > 0) {
      allocateAmountToTaxBandsForScotland(taxablePay, personalAllowancePerPeriod, calcPeriod, config)
    } else {
      allocateAmountToTaxBandsForAllButScotland(taxablePay, personalAllowancePerPeriod, calcPeriod, config)
    }

  private def allocateAmountToTaxBandsForAllButScotland(taxablePay: BigDecimal,
                                                        personalAllowancePerPeriod: BigDecimal,
                                                        calcPeriod: Periods.Period,
                                                        config: ESCTaxYearConfig) = {

    val higherRateCeilingPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxHigherBandUpperLimit, calcPeriod))
    val basicRateCapacityPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxBasicBandCapacity, calcPeriod))
    val basicRateCeiling : BigDecimal = basicRateCapacityPerPeriod + personalAllowancePerPeriod

    if (taxablePay <= personalAllowancePerPeriod) {
      CalculationTaxBands(zeroRateBand = taxablePay)

    } else if (taxablePay > personalAllowancePerPeriod && taxablePay <= basicRateCeiling){

      CalculationTaxBands(zeroRateBand = personalAllowancePerPeriod, basicRateBand = taxablePay - personalAllowancePerPeriod)

    } else if (taxablePay > basicRateCeiling && taxablePay <= higherRateCeilingPerPeriod) {

      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        basicRateBand = basicRateCeiling - personalAllowancePerPeriod,
        higherRateBand = taxablePay - basicRateCeiling
      )
    } else {
      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        basicRateBand = basicRateCeiling - personalAllowancePerPeriod,
        higherRateBand = higherRateCeilingPerPeriod - basicRateCeiling,
        additionalRateBand = taxablePay - higherRateCeilingPerPeriod
      )
    }
  }

  private def allocateAmountToTaxBandsForScotland(taxablePay: BigDecimal,
                                                  personalAllowancePerPeriod: BigDecimal,
                                                  calcPeriod: Periods.Period,
                                                  config: ESCTaxYearConfig) = {


    val starterRateCapacityPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxStarterBandCapacity, calcPeriod))
    val starterRateCeiling : BigDecimal = starterRateCapacityPerPeriod + personalAllowancePerPeriod

    val basicRateCapacityPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxBasicBandCapacity, calcPeriod))
    val basicRateCeiling : BigDecimal = basicRateCapacityPerPeriod + starterRateCeiling

    val intermediateCapacityPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxIntermediateBandCapacity, calcPeriod))
    val intermediateRateCeiling : BigDecimal = intermediateCapacityPerPeriod + basicRateCeiling

    val higherRateCeilingPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxHigherBandUpperLimit, calcPeriod))


    if (taxablePay <= personalAllowancePerPeriod) {
      CalculationTaxBands(zeroRateBand = taxablePay)

    } else if (taxablePay > personalAllowancePerPeriod && taxablePay <= starterRateCeiling) {
      CalculationTaxBands(zeroRateBand = personalAllowancePerPeriod, starterRateBand = taxablePay - personalAllowancePerPeriod)

    } else if (taxablePay > starterRateCeiling && taxablePay <= basicRateCeiling){
      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        starterRateBand = starterRateCeiling - personalAllowancePerPeriod,
        basicRateBand = taxablePay - starterRateCeiling)

    } else if (taxablePay > basicRateCeiling && taxablePay <= intermediateRateCeiling) {

      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        starterRateBand = starterRateCeiling - personalAllowancePerPeriod,
        basicRateBand = taxablePay - starterRateCeiling,
        intermediateRateBand = taxablePay - basicRateCeiling
      )
    } else if (taxablePay > intermediateRateCeiling && taxablePay <= higherRateCeilingPerPeriod) {

      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        starterRateBand = starterRateCeiling - personalAllowancePerPeriod,
        basicRateBand = taxablePay - starterRateCeiling,
        intermediateRateBand = taxablePay - basicRateCeiling,
        higherRateBand = taxablePay - intermediateRateCeiling
      )

    } else {
      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        starterRateBand = starterRateCeiling - personalAllowancePerPeriod,
        basicRateBand = taxablePay - starterRateCeiling,
        intermediateRateBand = taxablePay - basicRateCeiling,
        higherRateBand = taxablePay - intermediateRateCeiling,
        additionalRateBand = taxablePay - higherRateCeilingPerPeriod
      )
    }
  }


  def totalTaxDue(taxAmountPerBand: CalculationTaxBands, calcPeriod: Periods.Period): BigDecimal = {
    val annualTaxAmount = taxAmountPerBand.zeroRateBand +
      taxAmountPerBand.starterRateBand +
      taxAmountPerBand.basicRateBand +
      taxAmountPerBand.intermediateRateBand +
      taxAmountPerBand.higherRateBand +
      taxAmountPerBand.additionalRateBand

    round(annualAmountToPeriod(annualTaxAmount, calcPeriod))
  }

  protected def calculateTaxSavings(
                                     period: ESCPeriod,
                                     taxablePay: BigDecimal,
                                     personalAllowance: BigDecimal,
                                     reliefAmount: BigDecimal,
                                     calcPeriod: Periods.Period,
                                     taxCode: String,
                                     config: ESCTaxYearConfig
                                   ): (BigDecimal, BigDecimal, BigDecimal) = {

    val taxPerBandBeforeSacrifice: CalculationTaxBands =
      calculateTaxPerBand(allocateAmountToTaxBands(taxablePay, personalAllowance, period, calcPeriod, taxCode, config), period, config)

    val totalTaxDueBeforeSacrifice: BigDecimal = totalTaxDue(taxPerBandBeforeSacrifice, Periods.Yearly)
    val postSalarySacrificeEarnings: BigDecimal =  subtractActualReliefFromIncome(taxablePay, reliefAmount, Periods.Yearly)

    val taxPerBandAfterSacrifice: CalculationTaxBands =
      calculateTaxPerBand(allocateAmountToTaxBands(postSalarySacrificeEarnings, personalAllowance, period, calcPeriod, taxCode, config), period, config)

    val totalTaxDueAfterSacrifice: BigDecimal = totalTaxDue(taxPerBandAfterSacrifice, Periods.Yearly)

    //Total tax savings per one month
    val taxSavingAmountPerMonth = determineTotalSavings(totalTaxDueBeforeSacrifice, totalTaxDueAfterSacrifice)
    (taxSavingAmountPerMonth, totalTaxDueBeforeSacrifice, totalTaxDueAfterSacrifice)
  }

 def calculateTaxPerBand(taxableAmountPerBand : CalculationTaxBands,
                         period : ESCPeriod,
                         config :ESCTaxYearConfig) : CalculationTaxBands = {

    val personalAllowancePercent : BigDecimal = config.personalAllowanceRate
    val starterRatePercent:BigDecimal = config.taxStarterRate
    val basicRatePercent : BigDecimal = config.taxBasicRate
    val intermediateRatePercent: BigDecimal = config.taxIntermediateRate
    val higherRatePercent : BigDecimal = config.taxHigherRate
    val additionalRatePercent : BigDecimal = config.taxAdditionalRate

    CalculationTaxBands (
      zeroRateBand =  taxableAmountPerBand.zeroRateBand * (personalAllowancePercent/100),
      starterRateBand = taxableAmountPerBand.starterRateBand * (starterRatePercent / 100),
      basicRateBand = taxableAmountPerBand.basicRateBand * (basicRatePercent / 100),
      intermediateRateBand = taxableAmountPerBand.intermediateRateBand * (intermediateRatePercent / 100),
      higherRateBand = taxableAmountPerBand.higherRateBand * (higherRatePercent / 100),
      additionalRateBand = taxableAmountPerBand.additionalRateBand * (additionalRatePercent / 100)
    )
  }

  def allocateAmountToTaxBands(taxablePay: BigDecimal,
                                personalAllowancePerPeriod: BigDecimal,
                                period: ESCPeriod,
                                calcPeriod: Periods.Period,
                                taxCode: String,
                                config: ESCTaxYearConfig
                              ): CalculationTaxBands = {

    taxCode match {
      case "BR" => //20% band
        CalculationTaxBands(basicRateBand = taxablePay)
      case "D0" => //40% band
        CalculationTaxBands(higherRateBand = taxablePay)
      case "D1" => //45% band
        CalculationTaxBands(additionalRateBand = taxablePay)
      case "NT" => //No tax
        CalculationTaxBands()
      case _ =>
        allocateAmountToTaxBandsBasedOnTaxablePay(taxablePay, personalAllowancePerPeriod, calcPeriod, config)
    }
  }
}

trait ESCCalculatorNi extends ESCCalculatorHelpers {

  def allocateAmountToNIBands(grossPay: BigDecimal, period: ESCPeriod, config: ESCTaxYearConfig): CalculationNIBands = {
    val lowerEarningsLimit: BigDecimal = config.niCategory.lelMonthlyUpperLimitForCat
    val primaryEarningsLimit: BigDecimal = config.niCategory.lelPtMonthlyUpperLimitForCat
    val upperEarningsLimit: BigDecimal = config.niCategory.ptUelMonthlyUpperLimitForCat
    val lelPTBandCapacity: BigDecimal = primaryEarningsLimit - lowerEarningsLimit
    val ptUELBandCapacity: BigDecimal = upperEarningsLimit - primaryEarningsLimit

    if(grossPay <= lowerEarningsLimit) {
      CalculationNIBands(
        lowerEarningsBand = grossPay
      )
    }
    else if (grossPay > lowerEarningsLimit && grossPay <= primaryEarningsLimit) {
      CalculationNIBands(
        lowerEarningsBand = lowerEarningsLimit,
        primaryEarningsBand = grossPay - lowerEarningsLimit
      )
    }
    else if (grossPay > primaryEarningsLimit && grossPay <= upperEarningsLimit) {
      CalculationNIBands(
        lowerEarningsBand = lowerEarningsLimit,
        primaryEarningsBand = lelPTBandCapacity,
        upperEarningsBand = grossPay - (lowerEarningsLimit + lelPTBandCapacity)
      )
    }
    else {
      CalculationNIBands(
        lowerEarningsBand = lowerEarningsLimit,
        primaryEarningsBand = lelPTBandCapacity,
        upperEarningsBand = ptUELBandCapacity,
        aboveUpperEarningsBand = grossPay - (lowerEarningsLimit + lelPTBandCapacity + ptUELBandCapacity)
      )
    }
  }

  def calculateNIPerBand(grossAmountPerBand : CalculationNIBands, period : ESCPeriod, config: ESCTaxYearConfig) : CalculationNIBands = {
    val lelBandRate : BigDecimal = config.niCategory.lelRateForCat
    val lelPTBandRate : BigDecimal = config.niCategory.lelPtRateForCat
    val ptUELBandRate : BigDecimal = config.niCategory.ptUelRateForCat
    val aboveUelBandRate : BigDecimal = config.niCategory.aboveUelRateForCat

    CalculationNIBands (
      lowerEarningsBand =  grossAmountPerBand.lowerEarningsBand * (lelBandRate / 100),
      primaryEarningsBand = grossAmountPerBand.primaryEarningsBand * (lelPTBandRate / 100),
      upperEarningsBand = grossAmountPerBand.upperEarningsBand * (ptUELBandRate / 100),
      aboveUpperEarningsBand = grossAmountPerBand.aboveUpperEarningsBand * (aboveUelBandRate / 100)
    )
  }

  def totalNIDue(niAmountPerBand: CalculationNIBands, calcPeriod: Periods.Period): BigDecimal = {
    val annualNIAmount =
      niAmountPerBand.lowerEarningsBand +
        niAmountPerBand.primaryEarningsBand +
        niAmountPerBand.upperEarningsBand +
        niAmountPerBand.aboveUpperEarningsBand
    round(annualAmountToPeriod(annualNIAmount, calcPeriod))
  }

  def calculateNISavings(
                          period: ESCPeriod,
                          grossPay: BigDecimal,
                          reliefAmount: BigDecimal,
                          config: ESCTaxYearConfig,
                          calcPeriod: Periods.Period
                        ): (BigDecimal, BigDecimal, BigDecimal) = {
    val niPerBandBeforeSacrifice : CalculationNIBands = calculateNIPerBand(allocateAmountToNIBands(grossPay, period, config), period, config)
    val totalNIDueBeforeSacrifice : BigDecimal = totalNIDue(niPerBandBeforeSacrifice, Periods.Yearly)
    val postSalarySacrificeEarnings : BigDecimal =  subtractActualReliefFromIncome(grossPay, reliefAmount, Periods.Yearly)
    val niPerBandAfterSacrifice: CalculationNIBands = calculateNIPerBand(allocateAmountToNIBands(postSalarySacrificeEarnings, period, config), period, config)
    val totalNIDueAfterSacrifice: BigDecimal = totalNIDue(niPerBandAfterSacrifice, Periods.Yearly)
    //Total tax savings per one month
    val niSavingAmountPerMonth = determineTotalSavings(totalNIDueBeforeSacrifice, totalNIDueAfterSacrifice)

    (niSavingAmountPerMonth, totalNIDueBeforeSacrifice, totalNIDueAfterSacrifice)
  }
}

trait ESCCalculator extends ESCCalculatorTax with ESCCalculatorNi {

  import scala.concurrent.ExecutionContext.Implicits.global

  /**
    *
    * @param maximumReliefAmount maximum amount of vouchers user can buy
    * @param relevantEarnings    earnings from which user can buy vouchers
    * @param escAmountForPeriod  real amount that we want to assign to user
    * @return actual amount of vouchers that user can buy
    */

  def getActualRelief(maximumReliefAmount: BigDecimal, relevantEarnings: BigDecimal, escAmountForPeriod: BigDecimal): BigDecimal = {
    val maxLimit = if (maximumReliefAmount <= relevantEarnings) {
      maximumReliefAmount
    } else {
      relevantEarnings
    }

    if (escAmountForPeriod <= maxLimit) {
      escAmountForPeriod
    } else {
      maxLimit
    }
  }

  def determineSavingsPerClaimant(period: ESCPeriod, location: String): List[models.output.esc.ESCClaimant] = {

    val escAmountForPeriod: BigDecimal = round(period.children.filter(_.qualifying).foldLeft(BigDecimal(0.00))((acc, child) =>
      acc + amountToMonthlyAmount(child.childCareCost, child.childCareCostPeriod)))

    val periodParent: Option[ESCClaimant] = period.claimants.find(_.isPartner == false)
    val periodPartner: Option[ESCClaimant] = period.claimants.find(_.isPartner == true)

    val (parentESCTaxAmount, partnerESCTaxAmount, parentESCNIAmount, partnerESCNIAmount): (BigDecimal, BigDecimal, BigDecimal, BigDecimal) =
      (periodParent, periodPartner) match {

        case (Some(parent), Some(partner)) if parent.qualifying && partner.qualifying =>
                val claimantsByIncome: List[ESCClaimant] = period.claimants.sortBy(_.income.taxablePay)
                val (maximumReliefAmountHE, relevantEarningsForTaxHE, relevantEarningsForNIHE) =
                  getRelevantEarnings(period, claimantsByIncome.last.income, claimantsByIncome.last.isESCStartDateBefore2011, location)
                val (maximumReliefAmountLE, relevantEarningsForTaxLE, relevantEarningsForNILE) =
                  getRelevantEarnings(period, claimantsByIncome.head.income, claimantsByIncome.head.isESCStartDateBefore2011, location)
                val reliefFromTaxHE = getActualRelief(maximumReliefAmountHE, relevantEarningsForTaxHE, escAmountForPeriod)
                val reliefFromTaxLE = getActualRelief(maximumReliefAmountLE, relevantEarningsForTaxLE, escAmountForPeriod - reliefFromTaxHE)
                val reliefFromNIHE =
                  getActualRelief(maximumReliefAmountHE - reliefFromTaxHE, relevantEarningsForNIHE, escAmountForPeriod - reliefFromTaxHE - reliefFromTaxLE)
                val reliefFromNILE =
                  getActualRelief(maximumReliefAmountLE-reliefFromTaxLE, relevantEarningsForNILE, escAmountForPeriod-reliefFromTaxHE-reliefFromTaxLE-reliefFromNIHE)

                if (!claimantsByIncome.last.isPartner) {
                  (reliefFromTaxHE, reliefFromTaxLE, reliefFromNIHE, reliefFromNILE)
                } else {
                  (reliefFromTaxLE, reliefFromTaxHE, reliefFromNILE, reliefFromNIHE)
                }

        case (Some(parent), _) if parent.qualifying =>
          val (reliefFromTax, reliefFromNI) = determineRelief(period, location, parent, escAmountForPeriod)
          (reliefFromTax, BigDecimal(0.00), reliefFromNI, BigDecimal(0.00))

        case (_, Some(partner)) if partner.qualifying =>
          val (reliefFromTax, reliefFromNI) = determineRelief(period, location, partner, escAmountForPeriod)
          (BigDecimal(0.00), reliefFromTax, BigDecimal(0.00), reliefFromNI)

        case (_, _) => (BigDecimal(0.00), BigDecimal(0.00), BigDecimal(0.00), BigDecimal(0.00))
      }

    for (claimant <- period.claimants) yield {
      //Use monthly values for calculation
      val calcPeriod = Periods.Monthly
      val config = ESCConfig.getConfig(period.from, claimant.income.niCategory.toUpperCase.trim, location)
      val taxCode = getTaxCode(period, claimant.income, config)
      val personalAllowanceAmount: BigDecimal = getPersonalAllowance(period, claimant.income, config)

      val (actualTaxReliefAmount, actualNIReliefAmount): (BigDecimal, BigDecimal) = if (!claimant.isPartner) {
        (parentESCTaxAmount, parentESCTaxAmount + parentESCNIAmount)
      } else {
        (partnerESCTaxAmount, partnerESCTaxAmount + partnerESCNIAmount)
      }

      val taxablePayMonthly: BigDecimal = roundToPound(annualAmountToPeriod(claimant.income.taxablePay, calcPeriod))
      val grossPayMonthly: BigDecimal = roundToPound(annualAmountToPeriod(claimant.income.gross, calcPeriod))
      val personalAllowanceMonthly: BigDecimal = roundToPound(annualAmountToPeriod(personalAllowanceAmount, calcPeriod))

      val taxSavingAmounts: (BigDecimal, BigDecimal, BigDecimal) =
        calculateTaxSavings(
          period,
          taxablePayMonthly,
          personalAllowanceMonthly,
          actualTaxReliefAmount,
          calcPeriod,
          taxCode,
          config)

      val niSavingAmounts: (BigDecimal, BigDecimal, BigDecimal) = calculateNISavings(
        period,
        grossPayMonthly,
        actualNIReliefAmount,
        config,
        calcPeriod)

      populateClaimantModel(
        claimant.qualifying,
        eligibleMonths = claimant.eligibleMonthsInPeriod,
        isPartner = claimant.isPartner,
        taxablePay = claimant.income.taxablePay,
        grossPay = claimant.income.gross,
        taxCode = claimant.income.taxCode,
        niCategory = config.niCategory.niCategoryCode,
        vouchers = claimant.vouchers,
        escAmount = actualNIReliefAmount,
        escAmountPeriod = calcPeriod,
        escStartDate = claimant.escStartDate,
        totalSaving = taxSavingAmounts._1 + niSavingAmounts._1,
        taxSaving = taxSavingAmounts._1,
        niSaving = niSavingAmounts._1,
        taxPaidPreSacrifice = taxSavingAmounts._2,
        niPaidPreSacrifice = niSavingAmounts._2,
        taxPaidPostSacrifice = taxSavingAmounts._3,
        niPaidPostSacrifice = niSavingAmounts._3
      )
    }
  }

  def getPersonalAllowance(period: ESCPeriod, income: ESCTotalIncome, config: ESCTaxYearConfig) : BigDecimal =  {
    income.taxCode.trim match {
      case code if code.isEmpty => income.adjustPersonalAllowance(config.defaultPersonalAllowance)
      case _ => validateTaxCode(period, income)._1
    }
  }

  private def determineRelief(period: ESCPeriod, location: String, claimant: ESCClaimant, escAmountForPeriod: BigDecimal) = {
    val (maximumReliefAmount, relevantEarningsForTax, relevantEarningsForNI) =
      getRelevantEarnings(period, claimant.income, claimant.isESCStartDateBefore2011, location)
    val reliefFromTax = getActualRelief(maximumReliefAmount, relevantEarningsForTax, escAmountForPeriod)
    val reliefFromNI = getActualRelief(maximumReliefAmount - reliefFromTax, relevantEarningsForNI, escAmountForPeriod - reliefFromTax)
    (reliefFromTax, reliefFromNI)
  }

  private def determineCalculatedListOfClaimantsPairs(periods: List[ESCPeriod], location: String): List[models.output.esc.ESCClaimant] = {
    val listOfPairs = for(period <- periods) yield {
      //if one of the claimant income falls below the personal allowance then assign the entire childcare spend to the other claimant.
      determineSavingsPerClaimant(period, location)
    }

    listOfPairs.flatten
  }

  /**
    * @return Returns tuple of:
    *         - maximum amount of vouchers user can buy
    *         - relevant earnings between salary and personal allowance
    *         - relevant earnings between salary or personal allowance (whichever is less) and NI limit
    */
  protected def getRelevantEarnings(period: ESCPeriod, income: ESCTotalIncome, isESCStartDateBefore2011: Boolean, location: String) = {
    val config = ESCConfig.getConfig(period.from, income.niCategory.toUpperCase.trim, location)
    val taxCode = getTaxCode(period, income, config)
    // relevantEarningsForTax - range between salary and personal allowance
    // relevantEarningsForNI - range between salary or personal allowance (whichever is less) and NI limit
    val (relevantEarningsForTax, relevantEarningsForNI): (BigDecimal, BigDecimal) = getAnnualRelevantEarnings(income, period, config)
    // maximum limit of £243 a month
    val maximumReliefAmount: BigDecimal = determineMaximumIncomeRelief(
      isESCStartDateBefore2011,
      relevantEarningsForTax,
      Periods.Monthly,
      taxCode,
      config
    )
    (maximumReliefAmount, annualAmountToPeriod(relevantEarningsForTax, Periods.Monthly), annualAmountToPeriod(relevantEarningsForNI, Periods.Monthly))
  }

  private def accumulateAmount(qualified: Boolean, eligibleMonths: Int, amount: BigDecimal): BigDecimal = {
    if(qualified) {
      eligibleMonths * amount
    } else {
      BigDecimal(0)
    }
  }

  private def populateClaimant(claimantList: List[models.output.esc.ESCClaimant]): models.output.esc.ESCClaimant = {
    val eligibleMonths: Int = claimantList.foldLeft(0)((acc, c) => acc + c.eligibleMonthsInTaxYear)
    val qualification: Boolean = claimantList.exists(_.qualifying)
    val voucherFlag: Boolean = claimantList.exists(_.vouchers)

    val taxSavings: BigDecimal = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
      acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.savings.taxSaving)
    })

    val NISavings: BigDecimal = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
      acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.savings.niSaving)
    })

    val allTotalSavings: BigDecimal = taxSavings + NISavings
    val claimant = claimantList.head

    populateClaimantModel(
      qualification,
      eligibleMonths = eligibleMonths,
      claimant.isPartner,
      claimant.income.taxablePay,
      claimant.income.gross,
      claimant.income.taxCode,
      claimant.income.niCategory,
      voucherFlag,
      escAmount = claimantList.map(_.escAmount).max,
      escAmountPeriod = claimant.escAmountPeriod,
      escStartDate = claimant.escStartDate,
      totalSaving = allTotalSavings,
      taxSaving = taxSavings,
      niSaving = NISavings,
      taxPaidPreSacrifice = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
        acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.taxAndNIBeforeSacrifice.taxPaid)
      }),
      niPaidPreSacrifice = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
        acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.taxAndNIBeforeSacrifice.niPaid)
      }),
      taxPaidPostSacrifice = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
        acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.taxAndNIAfterSacrifice.taxPaid)
      }),
      niPaidPostSacrifice = claimantList.foldLeft(BigDecimal(0))((acc, c) => {
        acc + accumulateAmount(c.qualifying, c.eligibleMonthsInTaxYear, c.taxAndNIAfterSacrifice.niPaid)
      })
    )
  }

  private def determineClaimantsForTaxYear(listOfClaimants : List[models.output.esc.ESCClaimant]): List[models.output.esc.ESCClaimant] = {
    val overallParent = populateClaimant(listOfClaimants.filterNot(_.isPartner))
    val partnerExists: Boolean = listOfClaimants.exists(partner => partner.isPartner)

    partnerExists match {
      case false => List(overallParent)
      case true => List(overallParent, populateClaimant(listOfClaimants.filter(_.isPartner)))
    }
  }

  def getCalculatedTaxYears(escEligibility: ESCCalculatorInput): List[models.output.esc.ESCTaxYear] = {
    val taxYears = escEligibility.taxYears

    for(taxYear <- taxYears) yield {
      val claimantListForTY = determineCalculatedListOfClaimantsPairs(taxYear.periods, escEligibility.location)
      val resultClaimantList = determineClaimantsForTaxYear(claimantListForTY)

      val (overallTaxSavings, overallNISavings) = resultClaimantList.foldLeft(
        (BigDecimal(0.00), BigDecimal(0.00))
      )(
        (acc, claimant) => {
          val savings = claimant.savings
          (
            acc._1 + savings.taxSaving,
            acc._2 + savings.niSaving
          )
        }
      )

      models.output.esc.ESCTaxYear(
        from = taxYear.from,
        until = taxYear.until,
        totalSavings = models.output.esc.ESCSavings(
          totalSaving  = overallTaxSavings + overallNISavings,
          taxSaving = overallTaxSavings,
          niSaving = overallNISavings
        ),
        claimants = resultClaimantList
      )
    }
  }

  def award(eligibility: ESCCalculatorInput): Future[ESCCalculatorOutput] = {

    val calculatedTaxYears: List[models.output.esc.ESCTaxYear] = getCalculatedTaxYears(eligibility)

    val (taxSavings, niSavings, totalSavings) =
      calculatedTaxYears.foldLeft(
        (
          BigDecimal(0.00),
          BigDecimal(0.00),
          BigDecimal(0.00)
        )
      )(
        (acc, taxYear) => {
          val savings = taxYear.totalSavings
          (
            acc._1 + savings.taxSaving,
            acc._2 + savings.niSaving,
            acc._3 + savings.totalSaving
          )
        }
      )
    Future{
      ESCCalculatorOutput(
        from = eligibility.taxYears.head.from,
        until = eligibility.taxYears.last.until,
        totalSavings = ESCSavings(
          taxSaving = taxSavings,
          niSaving = niSavings,
          totalSaving = totalSavings
        ),
        taxYears = calculatedTaxYears
      )
    }
  }
}

object ESCCalculator extends ESCCalculator
