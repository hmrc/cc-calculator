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

import models.input.esc._
import models.output.esc.{ESCCalculation, Savings}
import models.utility.{CalculationNIBands, CalculationTaxBands}
import org.joda.time.LocalDate
import play.api.Logger
import utils.{MessagesObject, ESCConfig, ESCTaxYearConfig, Periods}
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

  def validateTaxCode(period : ESCPeriod, income : TotalIncome) : (BigDecimal, String) = { //TODO - Add validations for OT and K tax codes

    def validateCode(code : String) : Boolean =  (code.equals("BR") || code.equals("D0") || code.equals("D1") || code.equals("NT"))
    def validateCodeBasedOnEndsWith(code : String) : Boolean =  code.endsWith("L") || code.endsWith("M") ||
      code.endsWith("N") || code.endsWith("T") || code.endsWith("Y")

    income.taxCode.toUpperCase.trim match {
      case code if validateCode(code) =>
        (BigDecimal(0.00), code)
      case code if validateCodeBasedOnEndsWith(code) =>
        toInt(code.substring(0, code.length - 1)) match {
          case Some(number) =>
            (number * 10, code)
          case None =>
            Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.validateTaxCode - Exception case None")
            throw new NoSuchElementException(messages("cc.scheme.config.invalid.tax.code"))
        }
      case _ =>
        Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.validateTaxCode - Exception case others")
        throw new NoSuchElementException(messages("cc.scheme.config.invalid.tax.code"))
    }
  }

  def getPersonalAllowance(period : ESCPeriod, income : TotalIncome, config :ESCTaxYearConfig) : BigDecimal =  {
    income.taxCode.trim match {
      case codeAsString if codeAsString.trim.isEmpty =>
        income.adjustPersonalAllowance(config.defaultPersonalAllowance)
      case _ =>
        validateTaxCode(period, income)._1
    }
  }

  def getTaxCode(period : ESCPeriod, income : TotalIncome, config :ESCTaxYearConfig) : String = {
    income.taxCode.trim match {
      case codeAsString if codeAsString.trim.isEmpty =>
        config.defaultTaxCode
      case _ =>
        validateTaxCode(period, income)._2
    }
  }

  def getAnnualRelevantEarnings(income: TotalIncome, period : ESCPeriod, config :ESCTaxYearConfig) : BigDecimal = {
    val higherRateCeiling = config.taxHigherBandUpperLimit
    val personalAllowance = config.defaultPersonalAllowance
    income.gross match {
      case gross if gross <= personalAllowance =>
        BigDecimal(0.00)
      case gross if gross <= higherRateCeiling =>
        income.taxablePay - personalAllowance
      case gross if gross > higherRateCeiling =>
        income.taxablePay
    }
  }

  private def determineMaximumIncomeReliefPost2011BasedOnRelevantEarnings(relevantEarnings: BigDecimal,
                                                                          calcPeriod: Periods.Period = Periods.Monthly, config: ESCTaxYearConfig) = {
    val higherRateCeiling : BigDecimal = config.taxHigherBandUpperLimit
    val basicRateCeiling : BigDecimal = config.taxBasicBandCapacity
    relevantEarnings match {
      case amount if amount <= 0 =>
        BigDecimal (0.00) //0% band return 0
      case amount if amount <= basicRateCeiling => //20% band
        monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyBasic, calcPeriod)
      case amount if amount > basicRateCeiling && amount <= higherRateCeiling => //40% band
        monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyHigher, calcPeriod)
      case amount if amount > higherRateCeiling => //45% band
        monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyAdditional, calcPeriod)
    }
  }

  private def determineMaximumIncomeReliefPre2011(relevantEarnings: BigDecimal,
                                                  calcPeriod: Periods.Period = Periods.Monthly, taxCode: String) = {
    relevantEarnings match {
      case amount if amount <= 0 =>
        BigDecimal(0.00)
      case amount if amount > 0 =>
        taxCode.toUpperCase match {
          case "NT" => //No tax
            BigDecimal(0.00)
          case _ =>
            monthlyAmountToPeriod(pre2011MaxExemptionMonthly, calcPeriod)
        }
    }
  }

  def determineMaximumIncomeRelief(
                                    period : ESCPeriod,
                                    pre2011: Boolean,
                                    relevantEarnings: BigDecimal,
                                    calcPeriod: Periods.Period = Periods.Monthly,
                                    taxCode: String,
                                    config: ESCTaxYearConfig
                                  ): BigDecimal = {


    pre2011 match {
      case true =>
        determineMaximumIncomeReliefPre2011(relevantEarnings, calcPeriod, taxCode)

      case false =>
        taxCode.toUpperCase match {
          case "BR" => //20% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyBasic, calcPeriod)
          case "D0" => //40% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyHigher, calcPeriod)
          case "D1" => //45% band
            monthlyAmountToPeriod (config.post2011MaxExemptionMonthlyAdditional, calcPeriod)
          case "NT" => //No tax
            BigDecimal(0.00)
          case _ =>
            determineMaximumIncomeReliefPost2011BasedOnRelevantEarnings(relevantEarnings, calcPeriod, config)
        }
    }
  }

  def determineActualIncomeRelief(voucherAmount : BigDecimal, maxReliefAmount : BigDecimal) : BigDecimal = {
    voucherAmount match {
      case amount if amount <= maxReliefAmount =>
        amount
      case anyOtherAmount =>
        maxReliefAmount
    }
  }

  def subtractActualReliefFromIncome(income : BigDecimal, maximumRelief : BigDecimal, calcPeriod : Periods.Period) : BigDecimal = {
    val incomePerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(income, calcPeriod))

    incomePerPeriod match {
      case amount if amount <= maximumRelief =>
        BigDecimal(0.00)
      case amount =>
        amount - maximumRelief
    }
  }

  def determineTotalSavings(preSalarySacrificeAmount : BigDecimal, postSalarySacrificeAmount : BigDecimal) : BigDecimal = {
    preSalarySacrificeAmount match {
      case amount if amount <= postSalarySacrificeAmount =>
        BigDecimal(0.00)
      case amount =>
        amount - postSalarySacrificeAmount
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
                                       maximumRelief: BigDecimal,
                                       maximumReliefPeriod: Periods.Period,
                                       taxPaidPreSacrifice: BigDecimal,
                                       niPaidPreSacrifice: BigDecimal,
                                       taxPaidPostSacrifice: BigDecimal,
                                       niPaidPostSacrifice: BigDecimal
                                     ): models.output.esc.Claimant = {
    models.output.esc.Claimant(
      qualifying = qualifying,
      eligibleMonthsInTaxYear = eligibleMonths,
      isPartner = isPartner,
      income = models.output.esc.Income(taxablePay = taxablePay, gross = grossPay, taxCode = taxCode, niCategory = niCategory),
      vouchers = vouchers,
      escAmount = escAmount,
      escAmountPeriod = escAmountPeriod,
      escStartDate = escStartDate,
      savings = models.output.esc.Savings(totalSaving = totalSaving, taxSaving = taxSaving, niSaving = niSaving),
      maximumRelief = maximumRelief,
      maximumReliefPeriod = maximumReliefPeriod,
      taxAndNIBeforeSacrifice = models.output.esc.TaxAndNI(taxPaid = taxPaidPreSacrifice, niPaid = niPaidPreSacrifice),
      taxAndNIAfterSacrifice = models.output.esc.TaxAndNI(taxPaid = taxPaidPostSacrifice, niPaid = niPaidPostSacrifice)
    )
  }
}

trait ESCCalculatorTax extends ESCConfig with ESCCalculatorHelpers {

  private def allocateAmountToTaxBandsBasedOnTaxablePay(taxablePay: BigDecimal,  personalAllowancePerPeriod: BigDecimal,
                                                        calcPeriod: Periods.Period, config: ESCTaxYearConfig) : CalculationTaxBands = {

    val higherRateCeilingPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxHigherBandUpperLimit, calcPeriod))
    val basicRateCapacityPerPeriod : BigDecimal = roundToPound(annualAmountToPeriod(config.taxBasicBandCapacity, calcPeriod))

    val basicRateCeiling : BigDecimal = basicRateCapacityPerPeriod + personalAllowancePerPeriod

    if (taxablePay <= personalAllowancePerPeriod) {
      CalculationTaxBands(zeroRateBand = taxablePay)
    }
    else if (taxablePay > personalAllowancePerPeriod && taxablePay <= basicRateCeiling){
      CalculationTaxBands(zeroRateBand = personalAllowancePerPeriod, basicRateBand = taxablePay - personalAllowancePerPeriod)
    }
    else if (taxablePay > basicRateCeiling && taxablePay <= higherRateCeilingPerPeriod) {
      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        basicRateBand = basicRateCeiling - personalAllowancePerPeriod,
        higherRateBand = taxablePay - basicRateCeiling
      )
    }
    else {
      CalculationTaxBands(
        zeroRateBand = personalAllowancePerPeriod,
        basicRateBand = basicRateCeiling - personalAllowancePerPeriod,
        higherRateBand = higherRateCeilingPerPeriod - basicRateCeiling,
        additionalRateBand = taxablePay - higherRateCeilingPerPeriod
      )
    }
  }

  def allocateAmountToTaxBands(
                                taxablePay: BigDecimal,
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

  def calculateTaxPerBand(taxableAmountPerBand : CalculationTaxBands, period : ESCPeriod, config :ESCTaxYearConfig) : CalculationTaxBands = {
    val personalAllowancePercent : BigDecimal = config.personalAllowanceRate
    val basicRatePercent : BigDecimal = config.taxBasicRate
    val higherRatePercent : BigDecimal = config.taxHigherRate
    val additionalRatePercent : BigDecimal = config.taxAdditionalRate

    CalculationTaxBands (
      zeroRateBand =  taxableAmountPerBand.zeroRateBand * (personalAllowancePercent/100),
      basicRateBand = taxableAmountPerBand.basicRateBand * (basicRatePercent / 100),
      higherRateBand = taxableAmountPerBand.higherRateBand * (higherRatePercent / 100),
      additionalRateBand = taxableAmountPerBand.additionalRateBand * (additionalRatePercent / 100)
    )
  }

  def totalTaxDue(taxAmountPerBand: CalculationTaxBands, calcPeriod: Periods.Period): BigDecimal = {
    val annualTaxAmount = taxAmountPerBand.zeroRateBand + taxAmountPerBand.basicRateBand + taxAmountPerBand.higherRateBand +
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
}

trait ESCCalculatorNi extends ESCConfig with ESCCalculatorHelpers {

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

trait ESCCalculator extends ESCCalculatorHelpers with ESCCalculatorTax with ESCCalculatorNi {
  import scala.concurrent.ExecutionContext.Implicits.global

  def determineSavingsPerClaimant(claimants: List[Claimant], period: ESCPeriod): List[models.output.esc.Claimant] = {
    for(claimant <- claimants) yield {
      //Use monthly values for calculation
      val calcPeriod = Periods.Monthly
      val config = ESCConfig.getConfig(period.from, claimant.income.niCategory.toUpperCase.trim, claimant.location)
      val taxCode = getTaxCode(period, claimant.income, config)
      val relevantEarningsAmount: BigDecimal = getAnnualRelevantEarnings(claimant.income, period, config)
      val personalAllowanceAmount: BigDecimal = getPersonalAllowance(period, claimant.income, config)
      val maximumReliefAmount: BigDecimal =
        determineMaximumIncomeRelief(period, claimant.isESCStartDateBefore2011, relevantEarningsAmount, calcPeriod, taxCode, config)
      val actualReliefAmount: BigDecimal = determineActualIncomeRelief(claimant.escAmount, maximumReliefAmount)
      val taxablePayMonthly: BigDecimal = roundToPound(annualAmountToPeriod(claimant.income.taxablePay, calcPeriod))
      val grossPayMonthly: BigDecimal = roundToPound(annualAmountToPeriod(claimant.income.gross, calcPeriod))
      val personalAllowanceMonthly: BigDecimal = roundToPound(annualAmountToPeriod(personalAllowanceAmount, calcPeriod))
      val taxSavingAmounts: (BigDecimal, BigDecimal, BigDecimal) =
        calculateTaxSavings(period, taxablePayMonthly, personalAllowanceMonthly, actualReliefAmount, calcPeriod, taxCode, config)
      val niSavingAmounts: (BigDecimal, BigDecimal, BigDecimal) =
        calculateNISavings(period, grossPayMonthly, actualReliefAmount, config, calcPeriod)
      val totalSaving  = taxSavingAmounts._1 + niSavingAmounts._1

      populateClaimantModel(
        claimant.qualifying,
        eligibleMonths = claimant.eligibleMonthsInPeriod,
        isPartner = claimant.isPartner,
        taxablePay = claimant.income.taxablePay,
        grossPay = claimant.income.gross,
        taxCode = claimant.income.taxCode,
        niCategory = config.niCategory.niCategoryCode,
        vouchers = claimant.vouchers,
        escAmount = claimant.escAmount,
        escAmountPeriod = claimant.escAmountPeriod,
        escStartDate = claimant.escStartDate,
        totalSaving = totalSaving,
        taxSaving = taxSavingAmounts._1,
        niSaving = niSavingAmounts._1,
        maximumRelief = maximumReliefAmount,
        maximumReliefPeriod = calcPeriod,
        taxPaidPreSacrifice = taxSavingAmounts._2,
        niPaidPreSacrifice = niSavingAmounts._2,
        taxPaidPostSacrifice = taxSavingAmounts._3,
        niPaidPostSacrifice = niSavingAmounts._3
      )
    }
  }

  private def determineCalculatedListOfClaimantsPairs(periods: List[ESCPeriod]): List[models.output.esc.Claimant] = {
    val listOfPairs = for(period <- periods) yield {
      //if one of the claimant income falls below the personal allowance then assign the entire childcare spend to the other claimant.
      determineSavingsPerClaimant(createClaimantList(period), period)
    }
    listOfPairs.flatten
  }

  protected def calcReliefAmount(period: ESCPeriod, income: TotalIncome, isESCStartDateBefore2011: Boolean, escAmount: BigDecimal, location: String) = {
    val config = ESCConfig.getConfig(period.from, income.niCategory.toUpperCase.trim, location)
    val taxCode = getTaxCode(period, income, config)
    val personalAllowanceAmountMonthly: BigDecimal = annualAmountToPeriod(getPersonalAllowance(period, income, config), Periods.Monthly)
    val relevantEarningsAmount: BigDecimal = getAnnualRelevantEarnings(income, period, config)
    val maximumReliefAmount: BigDecimal = determineMaximumIncomeRelief(
      period,
      isESCStartDateBefore2011,
      relevantEarningsAmount,
      Periods.Monthly,
      taxCode,
      config
    )
    (personalAllowanceAmountMonthly, determineActualIncomeRelief(escAmount, maximumReliefAmount))
  }

  protected def selectClaimant(period: ESCPeriod, parent: Claimant, partner: Claimant): List[Claimant] = {
    val (parentPersonalAllowanceAmountMonthly, parentActualReliefAmount) =
      calcReliefAmount(period, parent.income, parent.isESCStartDateBefore2011, parent.escAmount, parent.location)
    val (partnerPersonalAllowanceAmountMonthly, partnerActualReliefAmount) =
      calcReliefAmount(period, partner.income, partner.isESCStartDateBefore2011, partner.escAmount, partner.location)
    (
      annualAmountToPeriod(parent.income.taxablePay, Periods.Monthly) - parentActualReliefAmount,
      annualAmountToPeriod(partner.income.taxablePay, Periods.Monthly) - partnerActualReliefAmount
    ) match {
      // parent income falls below personal allowance after the sacrifice assign the childcare spend to the partner
      case (parentIncome, partnerIncome) if (parentIncome <= parentPersonalAllowanceAmountMonthly && partnerIncome > partnerPersonalAllowanceAmountMonthly) =>
        List(parent.copy(escAmount = BigDecimal(0.00)), partner.copy(escAmount = partner.escAmount))
      case (parentIncome, partnerIncome) if (parentIncome > parentPersonalAllowanceAmountMonthly && partnerIncome <= partnerPersonalAllowanceAmountMonthly) =>
        List(parent.copy(escAmount = parent.escAmount), partner.copy(escAmount = BigDecimal(0.00)))
      case (parentIncome, partnerIncome) =>
        //adjust the childcarecost to 50% each only when both are qualified
        List(parent.copy(escAmount = parent.escAmount/2), partner.copy(escAmount = partner.escAmount/2))
    }
  }

  private def createClaimantList(period: ESCPeriod): List[Claimant]= {
    period.claimants.size match {
      case 2 => {
        val parent = period.claimants.head
        val partner = period.claimants.tail.head
        if (parent.qualifying && partner.qualifying) {
          selectClaimant(period, parent, partner)
        } else {
          if (parent.qualifying) {
            List(parent.copy(escAmount = parent.escAmount), partner.copy(escAmount = BigDecimal(0.00)))
          } else if (partner.qualifying) {
            List(parent.copy(escAmount = BigDecimal(0.00)), partner.copy(escAmount = partner.escAmount))
          } else {
            List(parent.copy(escAmount = BigDecimal(0.00)), partner.copy(escAmount = BigDecimal(0.00)))
          }
        }
      }
      case _ => period.claimants
    }
  }

  private def accumulateAmount(qualified: Boolean, eligibleMonths: Int, amount: BigDecimal): BigDecimal = {
    if(qualified) {
      eligibleMonths * amount
    } else {
      BigDecimal(0)
    }
  }

  private def populateClaimant(claimantList: List[models.output.esc.Claimant]): models.output.esc.Claimant = {
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
      claimant.maximumRelief,
      claimant.maximumReliefPeriod,
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

  private def determineClaimantsForTaxYear(listOfClaimants : List[models.output.esc.Claimant]): List[models.output.esc.Claimant] = {
    val overallParent = populateClaimant(listOfClaimants.filterNot(_.isPartner))
    val partnerExists: Boolean = listOfClaimants.exists(partner => partner.isPartner)
    partnerExists match {
      case false =>
        List(overallParent)
      case true =>
        val overallPartner = populateClaimant(listOfClaimants.filter(_.isPartner))
        List(overallParent, overallPartner)
    }
  }

  def getCalculatedTaxYears(taxYears: List[TaxYear]): List[models.output.esc.TaxYear] = {
    for(taxYear <- taxYears) yield {
      val claimantListForTY = determineCalculatedListOfClaimantsPairs(taxYear.periods)
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

      models.output.esc.TaxYear(
        from = taxYear.startDate,
        until = taxYear.endDate,
        totalSavings = models.output.esc.Savings(
          totalSaving  = overallTaxSavings + overallNISavings,
          taxSaving = overallTaxSavings,
          niSaving = overallNISavings
        ),
        claimants = resultClaimantList
      )
    }
  }

  def award(eligibility: ESCEligibility): Future[ESCCalculation] = {

    val calculatedTaxYears: List[models.output.esc.TaxYear] = getCalculatedTaxYears(eligibility.taxYears)

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
      ESCCalculation(
        from = eligibility.taxYears.head.startDate,
        until = eligibility.taxYears.last.endDate,
        totalSavings = Savings(
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
