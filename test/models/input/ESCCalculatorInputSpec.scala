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

package models.input

import calculators.ESCCalculator
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.esc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils._

class ESCCalculatorInputSpec extends UnitSpec with FakeCCCalculatorApplication {
  val location = "england"

  "ESC Input JSON" should {
    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/esc/input/calculator_input_test.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[ESCCalculatorInput]
      result match {
        case JsSuccess(x, _) => {
          x shouldBe a[ESCCalculatorInput]

          val taxYear = x.taxYears.head
          taxYear.startDate shouldBe a[LocalDate]
          taxYear.endDate shouldBe a[LocalDate]

          val period = taxYear.periods.head
          period shouldBe a[ESCPeriod]
          period.from shouldBe a[LocalDate]
          period.until shouldBe a[LocalDate]

          val claimant = period.claimants.head
          claimant shouldBe a[ESCClaimant]
          claimant.qualifying.isInstanceOf[Boolean] shouldBe true
          claimant.eligibleMonthsInPeriod.isInstanceOf[Int] shouldBe true
          claimant.isPartner.isInstanceOf[Boolean] shouldBe true
          claimant.escAmount shouldBe a[BigDecimal]
          claimant.escAmountPeriod.isInstanceOf[Periods.Period] shouldBe true
          claimant.escStartDate.isInstanceOf[LocalDate] shouldBe true

          val income = period.claimants.head.income
          income shouldBe a[ESCTotalIncome]
          income.gross shouldBe a[BigDecimal]
          income.taxablePay shouldBe a[BigDecimal]
          income.taxCode.isInstanceOf[String] shouldBe true
          income.niCategory.isInstanceOf[String] shouldBe true

          claimant.vouchers.isInstanceOf[Boolean] shouldBe true
        }
        case _ => throw new Exception
      }
    }
  }

  "ESCEligibility" should {

    "Assess ESC startdate where date is after 6 April 2011" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val escStartDate = LocalDate.parse("2016-06-01", formatter)
      val claimant = models.input.esc.ESCClaimant(qualifying = true, eligibleMonthsInPeriod = 2, isPartner = false, location = location, previousIncome = None, currentIncome = None, vouchers = true, escAmount = 200.00, escAmountPeriod = Periods.Monthly, escStartDate = escStartDate)
      claimant.isESCStartDateBefore2011 shouldBe false
    }

    "Assess ESC startdate where date is before 6 April 2011" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val escStartDate = LocalDate.parse("2010-06-01", formatter)
      val claimant = models.input.esc.ESCClaimant(qualifying = true, eligibleMonthsInPeriod = 2, isPartner = false, location = location, previousIncome = None, currentIncome = None, vouchers = true, escAmount = 200.00, escAmountPeriod = Periods.Monthly, escStartDate = escStartDate)
      claimant.isESCStartDateBefore2011 shouldBe true
    }

    "Assess ESC startdate where date is on 6 April 2011" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val escStartDate = LocalDate.parse("2011-04-06", formatter)
      val claimant = models.input.esc.ESCClaimant(qualifying = true, eligibleMonthsInPeriod = 2, isPartner = false, location = location, previousIncome = None, currentIncome = None, vouchers = true, escAmount = 200.00, escAmountPeriod = Periods.Monthly, escStartDate = escStartDate)
      claimant.isESCStartDateBefore2011 shouldBe false
    }
  }

  "models.input.esc.Period" should {

    "(TY 2017/2018) return the correct config for the current tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)
      val period = models.input.esc.ESCPeriod(from = periodStart, until = periodEnd, claimants = List())
      val niCat = NiCategory(
        niCategoryCode = "C",
        lelMonthlyLowerLimitForCat = 0.00,
        lelMonthlyUpperLimitForCat = 490.00,
        lelRateForCat = 0.00,
        lelPtMonthlyLowerLimitForCat = 491.00,
        lelPtMonthlyUpperLimitForCat = 680.00,
        lelPtRateForCat = 0.00,
        ptUelMonthlyLowerLimitForCat = 681.00,
        ptUelMonthlyUpperLimitForCat = 3753.00,
        ptUelRateForCat = 0.00,
        aboveUelMonthlyLowerLimitForCat = 3754.00,
        aboveUelRateForCat = 0.00
      )
      val taxYear = ESCTaxYearConfig(
        post2011MaxExemptionMonthlyBasic = 243.00,
        post2011MaxExemptionMonthlyHigher = 124.00,
        post2011MaxExemptionMonthlyAdditional = 110.00,
        defaultTaxCode = "1150L",
        personalAllowanceRate =0.00,
        defaultPersonalAllowance = 11500,
        taxBasicRate = 20.00,
        taxBasicBandCapacity = 33500.00,
        taxHigherRate = 40.00,
        taxHigherBandUpperLimit = 150000.00,
        taxAdditionalRate = 45.00,
        taxAdditionalBandLowerLimit= 150000.01,
        niCategory = niCat
      )
      ESCConfig.getConfig(period.from, "C", location) shouldBe taxYear
    }

    "(TY 2016/2017) return the correct config for the current tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2016-05-06", formatter)
      val periodEnd = LocalDate.parse("2017-04-05", formatter)
      val period = models.input.esc.ESCPeriod(from = periodStart, until = periodEnd, claimants = List())
      val niCat = NiCategory(
        niCategoryCode = "A",
        lelMonthlyLowerLimitForCat = 0.00,
        lelMonthlyUpperLimitForCat = 485.00,
        lelRateForCat = 0.00,
        lelPtMonthlyLowerLimitForCat = 486.00,
        lelPtMonthlyUpperLimitForCat = 672.00,
        lelPtRateForCat = 0.00,
        ptUelMonthlyLowerLimitForCat = 673.00,
        ptUelMonthlyUpperLimitForCat = 3583.00,
        ptUelRateForCat = 12.00,
        aboveUelMonthlyLowerLimitForCat = 3584.00,
        aboveUelRateForCat = 2.00
      )
      val taxYear = ESCTaxYearConfig(
        post2011MaxExemptionMonthlyBasic = 243.00,
        post2011MaxExemptionMonthlyHigher = 124.00,
        post2011MaxExemptionMonthlyAdditional = 110.00,
        defaultTaxCode = "1100L",
        personalAllowanceRate =0.00,
        defaultPersonalAllowance = 11000,
        taxBasicRate = 20.00,
        taxBasicBandCapacity = 32000.00,
        taxHigherRate = 40.00,
        taxHigherBandUpperLimit = 150000.00,
        taxAdditionalRate = 45.00,
        taxAdditionalBandLowerLimit= 150000.01,
        niCategory = niCat
      )
      ESCConfig.getConfig(period.from, "A", location) shouldBe taxYear
    }

    "(TY 2017/2018) return the correct config for the tax year with no income and ni category (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)
      val period = models.input.esc.ESCPeriod(from = periodStart, until = periodEnd, claimants = List())
      try {
        ESCConfig.getConfig(period.from, "", location) should not be a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

    "(TY2016) Throw exception when incorrect tax code is provided" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2016-05-06", formatter)
      val periodEnd = LocalDate.parse("2017-04-06", formatter)

      val service = ESCCalculator
      val period = ESCPeriod(from = periodStart, until = periodEnd, List())
      val income = ESCTotalIncome(taxablePay = BigDecimal(0.00), gross = BigDecimal(0.00), taxCode = "1201NJI", niCategory = "")
      try {
        val result = service.calculator.getPersonalAllowance(period, income, ESCConfig.getConfig(period.from, "", location))
        result shouldBe a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

  }

}
