/*
 * Copyright 2016 HM Revenue & Customs
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
import controllers.FakeCCCalculatorApplication
import models.input.APIModels.{Payload, Request}
import models.input.tc.TCEligibility._
import models.input.tc._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils._

/**
 * Created by adamconder on 07/07/15.
 */
class TCEligibilitySpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger {

  "TC Input JSON" should {
    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result match {
        case JsSuccess(x, _) => {
          x shouldBe a[Request]
          x.payload should not be null
          x.payload shouldBe a[Payload]
          x.payload.eligibility.tc.isInstanceOf[Option[TCEligibility]] shouldBe true

          x.payload.eligibility.tc.get.taxYears.isInstanceOf[List[TaxYear]] shouldBe true
          x.payload.eligibility.tc.get.proRataEnd.isInstanceOf[Option[LocalDate]] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.from shouldBe a[LocalDate]
          x.payload.eligibility.tc.get.taxYears.head.until shouldBe a[LocalDate]
          x.payload.eligibility.tc.get.taxYears.head.houseHoldIncome shouldBe a[BigDecimal]
          x.payload.eligibility.tc.get.taxYears.head.periods.isInstanceOf[List[Period]] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.periods.head.from shouldBe a[LocalDate]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.until shouldBe a[LocalDate]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements shouldBe a[HouseHoldElements]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.isInstanceOf[List[Claimant]] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.isInstanceOf[List[Child]] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.basic.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.childcare.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.family.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.hours30.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.loneParent.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.householdElements.secondParent.isInstanceOf[Boolean] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.qualifying.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.isPartner.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.claimantElements shouldBe a[ClaimantDisability]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.doesNotTaper.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.failures.isInstanceOf[Option[List[String]]] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.claimantElements.disability.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.claimants.head.claimantElements.severeDisability.isInstanceOf[Boolean] shouldBe true

          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.id.isInstanceOf[Short] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.name shouldBe a[String]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.qualifying.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childcareCost shouldBe a[BigDecimal]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childcareCostPeriod shouldBe a[Periods.Period]
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements shouldBe a[ChildElements]

          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements.child.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements.youngAdult.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements.childcare.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements.disability.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tc.get.taxYears.head.periods.head.children.head.childElements.severeDisability.isInstanceOf[Boolean] shouldBe true
        }
        case _ => throw new Exception
      }
    }
  }


  "TCEligibility" should {

    "Determine if we need childcare element for a period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2016-06-01",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31",formatter)
      val child = Child(id = 0, name = "Child 1", childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly, childElements = ChildElements())
      val period = Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(childcare = true), claimants = List(), children = List(child))
      period.getChildCareForPeriod shouldBe true
    }

    "return total household income" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2016-06-01",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31",formatter)
      val tcEligibility = TCEligibility(
        taxYears = List(TaxYear(
          from = periodStart,
          until = periodEnd,
          houseHoldIncome = BigDecimal(18000),
          periods = List()
        )))
      tcEligibility.taxYears.head.houseHoldIncome shouldBe BigDecimal(18000)
    }

    "return BigDecimal(0.00) when no houseHold income" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2016-06-01",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31",formatter)
      val tcEligibility = TCEligibility(
        taxYears = List(TaxYear(
          from = periodStart,
          until = periodEnd,
          houseHoldIncome = BigDecimal(0.00),
          periods = List()
        )))
      tcEligibility.taxYears.head.houseHoldIncome shouldBe BigDecimal(0.00)
    }

  }

  "models.input.tc.Period" should {

    "(TY 2016/2017) return the correct config for the current tax year (the tax year the period falls into 2016)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2016-05-06",formatter)
      val periodEnd = LocalDate.parse ("2016-04-05",formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      val tcTaxYear =  TCTaxYearConfig(
        wtc = WTC(
          basicElement =1960,
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
      val periodStart = LocalDate.parse ("2016-05-06",formatter)
      val periodEnd = LocalDate.parse ("2017-04-05",formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      val tcTaxYear =  TCTaxYearConfig(
        wtc = WTC(
          basicElement =1960,
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
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)
      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(), children = List())
      try {
        period.config should not be a[NoSuchElementException]
      } catch {
        case e : Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

    "(single claimant, not claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability(), failures = Some(List()))

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant), children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe false
    }

    "(single claimant, claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability(), failures = Some(List()))

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant), children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

    "(joint claimants, not claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability(), failures = Some(List()))
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability(), failures = Some(List()))

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2), children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe false
    }

    "(joint claimant, both claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability(), failures = Some(List()))
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability(), failures = Some(List()))

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2), children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

    "(joint claimant, one claiming) determine if a claimant is claiming social security benefit" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val periodStart = LocalDate.parse ("2017-05-06",formatter)
      val periodEnd = LocalDate.parse ("2018-04-05",formatter)

      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability(), failures = Some(List()))
      val claimant2 = models.input.tc.Claimant(qualifying = true, isPartner = false, claimantElements = ClaimantDisability(), failures = Some(List()))

      val period = models.input.tc.Period(from = periodStart, until = periodEnd, householdElements = HouseHoldElements(), claimants = List(claimant, claimant2), children = List())
      period.atLeastOneClaimantIsClaimingSocialSecurityBenefit shouldBe true
    }

  }

  "models.input.tc.Claimant" should {

    "(Claiming) determine if a claimant is claiming social security benefit" in {
      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = true, claimantElements = ClaimantDisability(), failures = Some(List()))
      claimant.isClaimingSocialSecurity shouldBe true
    }

    "(Not Claiming) determine if a claimant is claiming social security benefit" in {
      val claimant = models.input.tc.Claimant(qualifying = true, isPartner = false, doesNotTaper = false, claimantElements = ClaimantDisability(), failures = Some(List()))
      claimant.isClaimingSocialSecurity shouldBe false
    }

  }

  "models.input.tc.Eligibility" should {

    "(Proratering) determine if we need to prorata where prorata date is not specified" in {
      val tcEligibility = models.input.tc.TCEligibility(taxYears = List() , proRataEnd = None)
      tcEligibility.isProrateringRequired shouldBe false
    }

    "(Proratering) determine if we need to prorata where prorata date is specified" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val proRataDate = LocalDate.parse ("2016-05-06",formatter)

      val tcEligibility = models.input.tc.TCEligibility(taxYears = List() , proRataEnd = Some(proRataDate))
      tcEligibility.isProrateringRequired shouldBe true
    }
  }

  }
