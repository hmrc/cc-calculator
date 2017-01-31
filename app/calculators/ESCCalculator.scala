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
import models.input.esc._
import models.output.OutputAPIModel.AwardPeriod
import models.output.esc.{ESCCalculation, Savings}
import models.utility.{CalculationNIBands, CalculationTaxBands}
import org.joda.time.LocalDate
import play.api.Logger
import play.api.i18n.Messages.Implicits._
import play.api.Play.current
import play.api.i18n.Messages
import utils.{ESCConfig, ESCTaxYearConfig, Periods}

import scala.concurrent.Future
import scala.util.Success

object ESCCalculator extends ESCCalculator

trait ESCCalculator extends CCCalculator {

  val calculator = new ESCCalculatorService

  trait ESCCalculatorHelpers extends ESCConfig with CCCalculatorService {

    private def isEmpty(string : String) : Boolean = string.trim.isEmpty

    private def toInt(string : String) : Option[Int] = {
      try {
        Some(string.toInt)
      } catch {
        case e : Exception =>
          None
      }
    }


    def validateTaxCode(period : ESCPeriod, income : Income) : (BigDecimal, String) = { //TODO - Add validations for OT and K tax codes

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
              throw new NoSuchElementException(Messages("cc.scheme.config.invalid.tax.code"))
          }
        case _ =>
          Logger.warn(s"ESCCalculator.ESCCalculatorHelpers.validateTaxCode - Exception case others")
          throw new NoSuchElementException(Messages("cc.scheme.config.invalid.tax.code"))
      }
    }


    def getPersonalAllowance(period : ESCPeriod, income : Income, config :ESCTaxYearConfig) : BigDecimal =  {
      income.taxCode.trim match {
        case codeAsString if isEmpty(codeAsString) =>
          income.adjustPersonalAllowance(config.defaultPersonalAllowance)
        case _ =>
          validateTaxCode(period, income)._1
      }
    }

    def getTaxCode(period : ESCPeriod, income : Income, config :ESCTaxYearConfig) : String = {
      income.taxCode.trim match {
        case codeAsString if isEmpty(codeAsString) =>
          config.defaultTaxCode
        case _ =>
          validateTaxCode(period, income)._2
      }
    }

    def getAnnualRelevantEarnings(income: Income, period : ESCPeriod, config :ESCTaxYearConfig) : BigDecimal = {
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

    protected def sumClaimantEligibleMonths(listOfClaimants : List[models.output.esc.Claimant], f: models.output.esc.Claimant => Boolean) : Int = {
      listOfClaimants.foldLeft(0)((acc, c) => if (f(c)) acc + c.eligibleMonthsInTaxYear else acc)
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
        elements = models.output.esc.ClaimantElements(vouchers = vouchers),
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

  trait ESCCalculatorTax extends ESCConfig with ESCCalculatorHelpers with CCCalculatorService {

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
      val annualTaxAmount =
        taxAmountPerBand.zeroRateBand + taxAmountPerBand.basicRateBand + taxAmountPerBand.higherRateBand + taxAmountPerBand.additionalRateBand
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

  trait ESCCalculatorNi extends ESCConfig with ESCCalculatorHelpers with CCCalculatorService {

    def allocateAmountToNIBands(grossPay: BigDecimal, period: ESCPeriod, config: ESCTaxYearConfig): CalculationNIBands = {
      val lowerEarningsLimit: BigDecimal = config.niCategory.lelMonthlyUpperLimitForCat
      val primaryEarningsLimit: BigDecimal = config.niCategory.lelPtMonthlyUpperLimitForCat
      val upperAccrualEarningsLimit: BigDecimal = config.niCategory.ptUapMonthlyUpperLimitForCat
      val upperEarningsLimit: BigDecimal = config.niCategory.uapUelMonthlyUpperLimitForCat
      val lelPTBandCapacity: BigDecimal = primaryEarningsLimit - lowerEarningsLimit
      val ptUAPBandCapacity: BigDecimal = upperAccrualEarningsLimit - primaryEarningsLimit
      val uapUElBandCapacity: BigDecimal = upperEarningsLimit - upperAccrualEarningsLimit

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
      else if (grossPay > primaryEarningsLimit && grossPay <= upperAccrualEarningsLimit) {
        CalculationNIBands(
          lowerEarningsBand = lowerEarningsLimit,
          primaryEarningsBand = lelPTBandCapacity,
          upperAccrualEarningsBand = grossPay - (lowerEarningsLimit + lelPTBandCapacity)
        )
      }
      else if (grossPay > upperAccrualEarningsLimit && grossPay <= upperEarningsLimit) {
        CalculationNIBands(
          lowerEarningsBand = lowerEarningsLimit,
          primaryEarningsBand = lelPTBandCapacity,
          upperAccrualEarningsBand = ptUAPBandCapacity,
          upperEarningsBand = grossPay - (lowerEarningsLimit + lelPTBandCapacity + ptUAPBandCapacity)
        )
      }
      else {
        CalculationNIBands(
          lowerEarningsBand = lowerEarningsLimit,
          primaryEarningsBand = lelPTBandCapacity,
          upperAccrualEarningsBand = ptUAPBandCapacity,
          upperEarningsBand = uapUElBandCapacity,
          aboveUpperEarningsBand = grossPay - (lowerEarningsLimit + lelPTBandCapacity + ptUAPBandCapacity + uapUElBandCapacity)
        )
      }
    }

    def calculateNIPerBand(grossAmountPerBand : CalculationNIBands, period : ESCPeriod, config: ESCTaxYearConfig) : CalculationNIBands = {
      val lelBandRate : BigDecimal = config.niCategory.lelRateForCat
      val lelPTBandRate : BigDecimal = config.niCategory.lelPtRateForCat
      val ptUAPBandRate : BigDecimal = config.niCategory.ptUapRateForCat
      val uapUELBandRate : BigDecimal = config.niCategory.uapUelRateForCat
      val aboveUelBandRate : BigDecimal = config.niCategory.aboveUelRateForCat

      CalculationNIBands (
        lowerEarningsBand =  grossAmountPerBand.lowerEarningsBand * (lelBandRate / 100),
        primaryEarningsBand = grossAmountPerBand.primaryEarningsBand * (lelPTBandRate / 100),
        upperAccrualEarningsBand = grossAmountPerBand.upperAccrualEarningsBand * (ptUAPBandRate / 100),
        upperEarningsBand = grossAmountPerBand.upperEarningsBand * (uapUELBandRate / 100),
        aboveUpperEarningsBand = grossAmountPerBand.aboveUpperEarningsBand * (aboveUelBandRate / 100)
      )
    }

    def totalNIDue(niAmountPerBand: CalculationNIBands, calcPeriod: Periods.Period): BigDecimal = {
      val annualNIAmount =
        niAmountPerBand.lowerEarningsBand +
          niAmountPerBand.primaryEarningsBand +
          niAmountPerBand.upperAccrualEarningsBand +
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

      val niPerBandAfterSacrifice: CalculationNIBands =
        calculateNIPerBand(allocateAmountToNIBands(postSalarySacrificeEarnings, period, config), period, config)
      val totalNIDueAfterSacrifice: BigDecimal = totalNIDue(niPerBandAfterSacrifice, Periods.Yearly)

      //Total tax savings per one month
      val niSavingAmountPerMonth = determineTotalSavings(totalNIDueBeforeSacrifice, totalNIDueAfterSacrifice)

      (niSavingAmountPerMonth, totalNIDueBeforeSacrifice, totalNIDueAfterSacrifice)
    }
  }

  class ESCCalculatorService extends CCCalculatorService with ESCCalculatorHelpers with ESCCalculatorTax with ESCCalculatorNi {
    import scala.concurrent.ExecutionContext.Implicits.global

    def determineSavingsPerClaimant(claimants : List[Claimant], period : ESCPeriod) : List[models.output.esc.Claimant] = {
      for(claimant <- claimants) yield {
        //Use monthly values for calculation
        val calcPeriod = Periods.Monthly
        val config = ESCConfig.getConfig(period.from, claimant.income.niCategory.toUpperCase.trim)
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
          vouchers = claimant.elements.vouchers,
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

    private def determineCalculatedListOfClaimantsPairs(periods : List[ESCPeriod]) : List[models.output.esc.Claimant] = {
      val listOfPairs = for(period <- periods) yield {
        //if one of the claimant income falls below the personal allowance then assign the entire childcare spend to the other claimant.
        val claimantList = createClaimantList(period)
        determineSavingsPerClaimant(claimantList, period)
      }
      listOfPairs.flatten
    }

    private def createClaimantList(period : ESCPeriod) :  List[Claimant]= {
      val calcPeriod = Periods.Monthly
      def calcReliefAmount(income: Income, isESCStartDateBefore2011: Boolean, escAmount: BigDecimal) = {
        val config = ESCConfig.getConfig(period.from, income.niCategory.toUpperCase.trim)
        val taxCode = getTaxCode(period, income, config)
        val personalAllowanceAmountMonthly: BigDecimal = annualAmountToPeriod(getPersonalAllowance(period, income, config), calcPeriod)
        val relevantEarningsAmount: BigDecimal = getAnnualRelevantEarnings(income, period, config)
        val maximumReliefAmount: BigDecimal = determineMaximumIncomeRelief(
          period,
          isESCStartDateBefore2011,
          relevantEarningsAmount,
          calcPeriod,
          taxCode,
          config
        )
        (personalAllowanceAmountMonthly, determineActualIncomeRelief(escAmount, maximumReliefAmount))
      }
      def selectClaimant(parent: Claimant, partner: Claimant) = {
        val (parentPersonalAllowanceAmountMonthly, parentActualReliefAmount) =
          calcReliefAmount(parent.income, parent.isESCStartDateBefore2011, parent.escAmount)
        val (partnerPersonalAllowanceAmountMonthly, partnerActualReliefAmount) =
          calcReliefAmount(partner.income, partner.isESCStartDateBefore2011, partner.escAmount)
        (
          (annualAmountToPeriod(parent.income.taxablePay, calcPeriod) - parentActualReliefAmount),
          (annualAmountToPeriod(partner.income.taxablePay, calcPeriod) - partnerActualReliefAmount)
          ) match {
          // parent income falls below personal allowance after the sacrifice assign the childcare spend to the partner
          case (x, y) if (x <= parentPersonalAllowanceAmountMonthly && y > partnerPersonalAllowanceAmountMonthly) =>
            List(parent.copy(escAmount = BigDecimal(0.00)), partner.copy(escAmount = partner.escAmount * 2))
          case (x, y) if (x > parentPersonalAllowanceAmountMonthly && y <= partnerPersonalAllowanceAmountMonthly) =>
            List(parent.copy(escAmount = parent.escAmount * 2), partner.copy(escAmount = BigDecimal(0.00)))
          case (_, _) =>
            period.claimants
        }
      }
      period.claimants.size match {
        case 2 =>
          val parent = period.claimants.head
          val partner = period.claimants.tail.head
          if(parent.qualifying && partner.qualifying) {
            selectClaimant(parent, partner)
          }
          else {
            period.claimants
          }
        case _ => period.claimants
      }
    }

    private def determineClaimantsForTaxYear(listOfClaimantPairs : List[models.output.esc.Claimant]): List[models.output.esc.Claimant] = {
      def populateClaimant(isPartner: Boolean, claimant: models.output.esc.Claimant): models.output.esc.Claimant = {
        val eligibleMonths: Int = sumClaimantEligibleMonths(listOfClaimantPairs, (c: models.output.esc.Claimant) => c.isPartner == isPartner)
        val qualification: Boolean = listOfClaimantPairs.exists(claimant => if(claimant.isPartner == isPartner) claimant.qualifying else false)
        val voucherFlag: Boolean = listOfClaimantPairs.exists(claimant => if(claimant.isPartner == isPartner) claimant.elements.vouchers else false)
        val taxSavings: BigDecimal = eligibleMonths * claimant.savings.taxSaving
        val NISavings: BigDecimal = eligibleMonths * claimant.savings.niSaving
        val allTotalSavings: BigDecimal = taxSavings + NISavings

        populateClaimantModel(
          qualification,
          eligibleMonths = eligibleMonths,
          claimant.isPartner,
          claimant.income.taxablePay,
          claimant.income.gross,
          claimant.income.taxCode,
          claimant.income.niCategory,
          voucherFlag,
          claimant.escAmount,
          escAmountPeriod = claimant.escAmountPeriod,
          escStartDate = claimant.escStartDate,
          totalSaving = allTotalSavings,
          taxSavings,
          niSaving = NISavings,
          claimant.maximumRelief,
          claimant.maximumReliefPeriod,
          claimant.taxAndNIBeforeSacrifice.taxPaid,
          niPaidPreSacrifice = claimant.taxAndNIBeforeSacrifice.niPaid,
          claimant.taxAndNIAfterSacrifice.taxPaid,
          niPaidPostSacrifice = claimant.taxAndNIAfterSacrifice.niPaid
        )
      }
      val overallClaimant = populateClaimant(false, listOfClaimantPairs.head)

      val partnerExists: Boolean = listOfClaimantPairs.exists(partner => partner.isPartner)
      partnerExists match {
        case false =>
          List(overallClaimant)
        case true =>
          val overallPartner = populateClaimant(true, listOfClaimantPairs.tail.head)
          List(overallClaimant, overallPartner)
      }
    }

    def getCalculatedTaxYears(taxYears : List[TaxYear]) : List[models.output.esc.TaxYear] = {
      for(taxYear <- taxYears) yield {
        val claimantListForTY = determineCalculatedListOfClaimantsPairs(taxYear.periods)
        val resultClaimantList = determineClaimantsForTaxYear(claimantListForTY)
        val overallTaxSavings : BigDecimal = resultClaimantList.foldLeft(BigDecimal(0.00))((acc, claimant) => acc + claimant.savings.taxSaving)
        val overallNISavings : BigDecimal = resultClaimantList.foldLeft(BigDecimal(0.00))((acc, claimant) => acc + claimant.savings.niSaving)
        val overallTotalSavings : BigDecimal = overallTaxSavings + overallNISavings

        models.output.esc.TaxYear(
          from = taxYear.startDate,
          until = taxYear.endDate,
          totalSavings = models.output.esc.Savings(
            totalSaving  = overallTotalSavings,
            taxSaving = overallTaxSavings,
            niSaving = overallNISavings
          ),
          claimants = resultClaimantList
        )
      }
    }

    override def award(request : Request) : Future[AwardPeriod] = {

      def getESCCalculation(eligibility: ESCEligibility) : ESCCalculation = {
        val calculatedTaxYears : List[models.output.esc.TaxYear] = getCalculatedTaxYears(eligibility.taxYears)

        ESCCalculation(
          from = eligibility.taxYears.head.startDate,
          until = eligibility.taxYears.last.endDate,
          totalSavings = Savings(
            taxSaving = calculatedTaxYears.foldLeft(BigDecimal(0.00))((acc, taxYear) => acc + taxYear.totalSavings.taxSaving),
            niSaving = calculatedTaxYears.foldLeft(BigDecimal(0.00))((acc, taxYear) => acc + taxYear.totalSavings.niSaving),
            totalSaving = calculatedTaxYears.foldLeft(BigDecimal(0.00))((acc, taxYear) => acc + taxYear.totalSavings.totalSaving)
          ),
          taxYears = calculatedTaxYears
        )
      }

      Future{
        request.getESCEligibility match {
          case Success(result) =>
            AwardPeriod(esc = Some(getESCCalculation(result)))
          case _ =>
            AwardPeriod()
        }
      }
    }
  }
}
