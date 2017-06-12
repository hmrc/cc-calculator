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

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.tc.TCEligibility._
import models.input.tc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils._

class TCEligibilitySpec extends UnitSpec with FakeCCCalculatorApplication {

  "TC Input JSON" should {
    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCEligibility]
      result.get shouldBe a[TCEligibility]

      result.get.taxYears.isInstanceOf[List[TaxYear]] shouldBe true

      val taxYear = result.get.taxYears.head
      taxYear.from shouldBe a[LocalDate]
      taxYear.until shouldBe a[LocalDate]
      taxYear.previousHouseholdIncome shouldBe a[Income]
      taxYear.currentHouseholdIncome shouldBe a[Income]
      taxYear.periods.isInstanceOf[List[Period]] shouldBe true

      val period = taxYear.periods.head
      period.from shouldBe a[LocalDate]
      period.until shouldBe a[LocalDate]
      period.householdElements shouldBe a[HouseHoldElements]
      period.claimants.isInstanceOf[List[Claimant]] shouldBe true
      period.children.isInstanceOf[List[Child]] shouldBe true

      val householdElaments = period.householdElements
      householdElaments.basic.isInstanceOf[Boolean] shouldBe true
      householdElaments.childcare.isInstanceOf[Boolean] shouldBe true
      householdElaments.family.isInstanceOf[Boolean] shouldBe true
      householdElaments.hours30.isInstanceOf[Boolean] shouldBe true
      householdElaments.loneParent.isInstanceOf[Boolean] shouldBe true
      householdElaments.secondParent.isInstanceOf[Boolean] shouldBe true

      val claimant = period.claimants.head
      claimant.qualifying.isInstanceOf[Boolean] shouldBe true
      claimant.isPartner.isInstanceOf[Boolean] shouldBe true
      claimant.claimantElements shouldBe a[ClaimantDisability]
      claimant.doesNotTaper.isInstanceOf[Boolean] shouldBe true

      claimant.claimantElements.disability.isInstanceOf[Boolean] shouldBe true
      claimant.claimantElements.severeDisability.isInstanceOf[Boolean] shouldBe true

      val child = period.children.head
      child.qualifying.isInstanceOf[Boolean] shouldBe true
      child.childcareCost shouldBe a[BigDecimal]
      child.childcareCostPeriod shouldBe a[Periods.Period]

      val childElements = child.childElements
      childElements shouldBe a[ChildElements]
      childElements.child.isInstanceOf[Boolean] shouldBe true
      childElements.youngAdult.isInstanceOf[Boolean] shouldBe true
      childElements.childcare.isInstanceOf[Boolean] shouldBe true
      childElements.disability.isInstanceOf[Boolean] shouldBe true
      childElements.severeDisability.isInstanceOf[Boolean] shouldBe true

    }
  }


  "TCEligibility" should {

    "Determine if we need childcare element for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2016-06-01", formatter)
      val periodEnd = LocalDate.parse("2016-08-31", formatter)
      val child = Child(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, childElements = ChildElements())
      val period = Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(childcare = true), claimants = List(),
        children = List(child))
      period.getChildCareForPeriod shouldBe true
    }
  }

    //TODO: Proper income testing
//    "return total household income" in {
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val periodStart = LocalDate.parse ("2016-06-01",formatter)
//      val periodEnd = LocalDate.parse ("2016-08-31",formatter)
//      val tcEligibility = TCEligibility(
//        taxYears = List(TaxYear(
//          from = periodStart,
//          until = periodEnd,
//          previousHouseholdIncome = Income,
//          currentHouseholdIncome = Income,
//          periods = List()
//        )))
//      tcEligibility.taxYears.head.houseHoldIncome shouldBe BigDecimal(18000)
//    }
//
//    "return BigDecimal(0.00) when no houseHold income" in {
//      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
//      val periodStart = LocalDate.parse ("2016-06-01",formatter)
//      val periodEnd = LocalDate.parse ("2016-08-31",formatter)
//      val tcEligibility = TCEligibility(
//        taxYears = List(TaxYear(
//          from = periodStart,
//          until = periodEnd,
//          houseHoldIncome = BigDecimal(0.00),
//          periods = List()
//        )))
//      tcEligibility.taxYears.head.houseHoldIncome shouldBe BigDecimal(0.00)
//    }
//
//  }

  "models.input.tc.Period" should {

    "(TY 2016/2017) return the correct config for the current tax year (the tax year the period falls into 2016)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2016-05-06", formatter)
      val periodEnd = LocalDate.parse("2016-04-05", formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      val tcTaxYear = TCTaxYearConfig(
        otherIncomeAdjustment = 0,
        currentIncomeFallDifferenceAmount = 2500,
        currentIncomeRiseDifferenceAmount = 2500,
        wtc = WTC(
          basicElement = 1960,
          coupleElement = 2010,
          loneParentElement = 2010,
          hours30Element = 810,
          disabledWorkerElement = 2970,
          severeDisabilityWorkerElement = 1275,
          maxChildcareOneChildElement = 175,
          maxChildcareMoreChildrenElement = 300,
          eligibleCostCoveredPercent = 70),
        ctc = CTC(
          childElement = 2780,
          youngPersonElement = 2780,
          disabledChildElement = 3140,
          severeDisabilityChildElement = 1275,
          familyElement = 545),
        thresholds = Thresholds(
          wtcIncomeThreshold = 6420,
          ctcIncomeThreshold = 16105,
          taperRatePercent = 41))
      period.config shouldBe tcTaxYear
      period.config.thresholds.wtcIncomeThreshold shouldBe 6420
      period.config.thresholds.taperRatePercent shouldBe 41
    }

    "(TY 2016/2017) return the correct config for the current tax year (the tax year the period falls across 2016-2017)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2016-05-06", formatter)
      val periodEnd = LocalDate.parse("2017-04-05", formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      val tcTaxYear = TCTaxYearConfig(
        otherIncomeAdjustment = 0,
        currentIncomeFallDifferenceAmount = 2500,
        currentIncomeRiseDifferenceAmount = 2500,
        wtc = WTC(
          basicElement = 1960,
          coupleElement = 2010,
          loneParentElement = 2010,
          hours30Element = 810,
          disabledWorkerElement = 2970,
          severeDisabilityWorkerElement = 1275,
          maxChildcareOneChildElement = 175,
          maxChildcareMoreChildrenElement = 300,
          eligibleCostCoveredPercent = 70),
        ctc = CTC(
          childElement = 2780,
          youngPersonElement = 2780,
          disabledChildElement = 3140,
          severeDisabilityChildElement = 1275,
          familyElement = 545),
        thresholds = Thresholds(
          wtcIncomeThreshold = 6420,
          ctcIncomeThreshold = 16105,
          taperRatePercent = 41))
      period.config shouldBe tcTaxYear
      period.config.thresholds.wtcIncomeThreshold shouldBe 6420
      period.config.thresholds.taperRatePercent shouldBe 41
    }

    "(TY 2017/2018) return the correct config for the current tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      try {
        period.config should not be a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

    "(single claimant, not claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability())

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant),
        children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe false
    }

    "(single claimant, claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability())

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant),
        children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

    "(joint claimants, not claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability())
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability())

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2),
        children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe false
    }

    "(joint claimant, both claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability())
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability())

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2),
        children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

    "(joint claimant, one claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse("2017-05-06", formatter)
      val periodEnd = LocalDate.parse("2018-04-05", formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability())
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability())

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2),
        children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

  }

  "models.input.tc.Claimant" should {

    "(Claiming) determine if a claimant is claiming social security benefit" in {
      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability())
      claimant.doesNotTaper shouldBe true
    }

    "(Not Claiming) determine if a claimant is claiming social security benefit" in {
      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = false, claimantElements = ClaimantDisability())
      claimant.doesNotTaper shouldBe false
    }

  }
}

