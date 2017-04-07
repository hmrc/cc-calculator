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

import calculators.TFCCalculator.TFCCalculatorService
import models.input.APIModels.{Eligibility, Payload, Request}
import models.input.tfc.{Child, Disability, TFCEligibility}
import models.output.OutputAPIModel.AwardPeriod
import models.output.tfc.{Contribution, OutputChild, TFCPeriod}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import uk.gov.hmrc.play.test.UnitSpec
import utils.FakeCCCalculatorApplication

import scala.concurrent.Future


class TFCCalculatorSpec extends UnitSpec with FakeCCCalculatorApplication {

  "TFCCalculatorService" should {

    "return an instance of TFCCalculatorService" in {
      val service = TFCCalculator
      service.calculator shouldBe a[TFCCalculatorService]
    }

    "return a Future[AwardPeriod] result" in {
      val service = TFCCalculator
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)

      val result = service.calculator.award(Request(payload = Payload(eligibility = Eligibility(tfc = Some(TFCEligibility(from = fromDate, until = untilDate, householdEligibility = true, periods = List())), tc = None, esc = None))))
      result.isInstanceOf[Future[AwardPeriod]] shouldBe true
    }

    "Generate total award with claimants (Total Award test - empty award period)" in {
      val emptyRequest = Request(payload = Payload(eligibility = Eligibility(null, null, null)))
      val result: AwardPeriod = TFCCalculator.calculator.award(emptyRequest)

      result shouldBe AwardPeriod()
    }

    "return a calculated TFC for 1 TFC period with 1 child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
      val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(200.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = List(child))
      val result = TFCCalculator.calculator.getCalculatedTFCPeriods(List(tfcPeriod))

      result shouldBe List(
        TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodContribution = Contribution(600.0,0.0,600.0),
          children = List(
            OutputChild(
              id = 2,
              name = Some("Child 1"),
              childCareCost =  BigDecimal(200.00),
              childContribution = Contribution(600.0,0.0,600.0),
              timeToMaximizeTopUp = 0,
              failures = List()
            )
          )
        )
      )
    }

    "return a calculated TFC for 1 TFC period with 2 children" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
      val children = List(
        Child(
          id = 2,
          name = Some("Child 1"),
          childcareCost = BigDecimal(200.00),
          qualifying = true,
          from = Some(fromDate),
          until  = Some(untilDate),
          disability = Disability(disabled = false, severelyDisabled = false)
        ),
        Child(
          id = 2,
          name = Some("Child 2"),
          childcareCost = BigDecimal(800.00),
          qualifying = true,
          from = Some(fromDate),
          until  = Some(untilDate),
          disability = Disability(disabled = false, severelyDisabled = false)
        )
      )
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = children)
      val result = TFCCalculator.calculator.getCalculatedTFCPeriods(List(tfcPeriod))

      result shouldBe List(
        TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodContribution = Contribution(3000.00,0.00,3000.00),
          children = List(
            OutputChild(
              id = 2,
              name = Some("Child 1"),
              childCareCost =  BigDecimal(200.00),
              childContribution = Contribution(600.00,0.00,600.00),
              timeToMaximizeTopUp = 0,
              failures = List()
            ),
            OutputChild(
              id = 2,
              name = Some("Child 2"),
              childCareCost =  BigDecimal(800.00),
              childContribution = Contribution(2400.00,0.00,2400.00),
              timeToMaximizeTopUp = 0,
              failures = List()
            )
          )
        )
      )
    }

    "return a calculated TFC for 1 TFC period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = List())
      val result = TFCCalculator.calculator.getCalculatedTFCPeriods(List(tfcPeriod))

      result shouldBe List(
        TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodContribution = Contribution(0.00,0.00,0.00),
          children = List()
        )
      )
    }

    "return a calculated TFC for 2 TFC periods" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
      val tfcPeriodList = List(
        models.input.tfc.TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodEligibility = false,
          children = List()
        ),
        models.input.tfc.TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodEligibility = false,
          children = List()
        )
      )
      val result = TFCCalculator.calculator.getCalculatedTFCPeriods(tfcPeriodList)

      result shouldBe List(
        TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodContribution = Contribution(),
          children = List()
        ),
        TFCPeriod(
          from = fromDate,
          until = untilDate,
          periodContribution = Contribution(),
          children = List()
        )
      )
    }
    "return a output child for 1 child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-08-27T00:00:00", formatter)
      val untilDate = LocalDate.parse("2016-11-27T00:00:00", formatter)
      val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(200.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = List(child))
      val result = TFCCalculator.calculator.getOutputChildren(tfcPeriod)

      result shouldBe List(
        OutputChild(
          id = 2,
          name = Some("Child 1"),
          childCareCost =  BigDecimal(200.00),
          childContribution = Contribution(600.00,0.00,600.00),
          timeToMaximizeTopUp = 0,
          failures = List()
        )
      )
    }

    "return a child name for non-empty name" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-08-27T00:00:00", formatter)
      val untilDate = LocalDate.parse("2016-11-27T00:00:00", formatter)
      val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(200.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
      val outputChild = TFCCalculator.calculator.getOutputChildren(tfcPeriod)
      outputChild.head.name shouldBe Some("Child 1")
    }

    "return a childcareCost for cost > 0.00" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-08-27T00:00:00", formatter)
      val untilDate = LocalDate.parse("2016-11-27T00:00:00", formatter)
      val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(500.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
      val outputChild = TFCCalculator.calculator.getOutputChildren(tfcPeriod)
      outputChild.head.childCareCost shouldBe BigDecimal(500.00)
    }


    "return number of days between two dates" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
      val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(Some(fromDate), Some(untilDate))
      result shouldBe 20
    }

    "return number of days in a TFCPeriod" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
      val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
      val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(Some(fromDate), Some(untilDate))
      result shouldBe 92
    }

    "return exception  when until date is present and from date is null" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = null
      val untilDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
      try {
        val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(fromDate, Some(untilDate))
        result shouldBe a[IllegalArgumentException]
      }
      catch {
        case e: Exception =>
          e shouldBe a[IllegalArgumentException]
      }
    }

    "return exception  when start and until date is null" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = null
      val untilDate = null
      try
      {
        val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(fromDate, untilDate)
        result shouldBe a[IllegalArgumentException]
      }
      catch {
        case e: Exception =>
          e shouldBe a[IllegalArgumentException]
      }
    }

    "return exception when from date is present but until date is null" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
      val untilDate = null
      try {
        val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(Some(fromDate), untilDate)
        result shouldBe a[IllegalArgumentException]
      }
      catch {
        case e: Exception =>
          e shouldBe a[IllegalArgumentException]
      }
    }

    "return exception when from date is any and until date is any" in {
      try {
        val result = TFCCalculator.calculator.getChildQualifyingDaysInTFCPeriod(None, None)
        result shouldBe a[IllegalArgumentException]
      }
      catch {
        case e: Exception =>
          e shouldBe a[IllegalArgumentException]
      }
    }

    "return childcare cost for a period" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(500.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val result = TFCCalculator.calculator.getChildCareCostForPeriod(child)
        result shouldBe BigDecimal(1500.00)
      }

    "return childcare cost for a period for 0 spend" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(0.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val result = TFCCalculator.calculator.getChildCareCostForPeriod(child)
        result shouldBe BigDecimal(0.00)
      }

    "return topUpPercent of child care cost for a period for current year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(500.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getTopUpPercentForChildCareCost(child, tfcPeriod.configRule)
        result shouldBe BigDecimal(300.00)
      }

    "return topUpPercent of child care cost for a period - disabled for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(500.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getTopUpPercentForChildCareCost(child, tfcPeriod.configRule)
        result shouldBe BigDecimal(300.00)
      }

    "return topUpPercent of child care cost for a period for zero amount" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(0.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getTopUpPercentForChildCareCost(child, tfcPeriod.configRule)
        result shouldBe BigDecimal(0.00)
      }

    "return maximum topup  when child care monthly cost 0)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(0.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(0.00)
      }

    "return maximum topup  when child care monthly cost 700)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(700.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(420.00)
      }


    "return maximum topup  when child care monthly cost 833.34)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(833.34), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(500.00)
      }


    "return maximum topup  when child care monthly cost 1000)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1000.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(500.00)
      }

    "return maximum topup  when child care monthly cost 0, child disabled)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(0.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(0.00)
      }

    "return maximum topup  when child care monthly cost 1000, child disabled)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1000.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(600.00)
      }

    "return maximum topup  when child care monthly cost 1666.67, child disabled)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1666.67), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(1000.00)
      }

    "return maximum topUp  when child care monthly cost 5500, child disabled)" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1800.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val tfcConfig = tfcPeriod.configRule
        val result = TFCCalculator.calculator.getMaximumTopup(child, tfcConfig)
        result shouldBe BigDecimal(1000.00)
      }

    "return calculated contribution for a child with monthly childcare cost 500 for current year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-07-06T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(500.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(1483.69),
            government =  BigDecimal(16.31),
            totalChildCareSpend =  BigDecimal(1500.00)
          )
      }

    "return calculated contribution for a child with monthly childcare cost 1000 for current year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1000.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(2500.00),
            government =  BigDecimal(500.00),
            totalChildCareSpend =  BigDecimal(3000.00)
          )
      }

    "return calculated contribution for a child with monthly childcare cost 833 for current year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-07-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-10-01T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(833.33), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(1999.99),
            government =  BigDecimal(500.00),
            totalChildCareSpend =  BigDecimal(2499.99)
          )
      }

    "return calculated contribution for a child with monthly childcare cost 600 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-11-30T18:46:17", formatter)
        val untilDate = LocalDate.parse("2018-02-28T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(600.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 90, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(1440.00),
            government =  BigDecimal(360.00),
            totalChildCareSpend =  BigDecimal(1800.00)
          )
      }
    "return calculated contribution for a disabled child with monthly childcare cost 1000 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-11-30T18:46:17", formatter)
        val untilDate = LocalDate.parse("2018-02-28T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1000.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 90, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(2400.00),
            government =  BigDecimal(600.00),
            totalChildCareSpend =  BigDecimal(3000.00)
          )
      }

    "return calculated contribution for a disabled child with monthly childcare cost 2000 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-05-31T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-08-31T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(2000.00), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(5000.00),
            government =  BigDecimal(1000.00),
            totalChildCareSpend =  BigDecimal(6000.00)
          )
      }

    "return calculated contribution for a disabled child with monthly childcare cost 1666.66 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-05-31T18:46:17", formatter)
        val untilDate = LocalDate.parse("2017-08-31T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1666.66), qualifying = true, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(3999.98),
            government =  BigDecimal(1000.00),
            totalChildCareSpend =  BigDecimal(4999.98)
          )
      }

    "return calculated contribution for a child not qualifying with monthly childcare cost 600 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-11-30T18:46:17", formatter)
        val untilDate = LocalDate.parse("2018-02-28T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(600.00), qualifying = false, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = false, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 90, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(1800.00),
            government =  BigDecimal(0.00),
            totalChildCareSpend =  BigDecimal(1800.00)
          )
      }

    "return calculated contribution for a disabled child not qualifying with monthly childcare cost 1000 for next year" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2017-05-31T18:46:17", formatter)
        val untilDate = LocalDate.parse("2018-08-31T18:46:17", formatter)
        val child = Child(id = 2, name = Some("Child 1"), childcareCost = BigDecimal(1000.00), qualifying = false, from = Some(fromDate), until  = Some(untilDate),disability = Disability(disabled = true, severelyDisabled = false))
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = true, children = List(child))
        val result = TFCCalculator.calculator.getChildContribution(child, tfcPeriod.configRule, 92, true)
        result shouldBe
          Contribution(
            parent =  BigDecimal(3000.00),
            government =  BigDecimal(0.00),
            totalChildCareSpend =  BigDecimal(3000.00)
          )
      }

    "return output children for 2 children where 1 child is qualifying" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
        val children = List(
          Child(
            id = 2,
            name = Some("Child 1"),
            childcareCost = BigDecimal(200.00),
            qualifying = true,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = false, severelyDisabled = false)
          ),
          Child(
            id = 2,
            name = Some("Child 2"),
            childcareCost = BigDecimal(800.00),
            qualifying = false,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = false, severelyDisabled = false)
          )
        )
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = children)
        val result = TFCCalculator.calculator.getOutputChildren(tfcPeriod)

        result shouldBe List(
          OutputChild(
            id = 2,
            name = Some("Child 1"),
            childCareCost =  BigDecimal(200.00),
            childContribution = Contribution(
              parent = 600.00,
              government = 0.00,
              totalChildCareSpend = 600.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          ),
          OutputChild(
            id = 2,
            name = Some("Child 2"),
            childCareCost =  BigDecimal(800.00),
            childContribution = Contribution(
              parent = 2400.00,
              government = 00.00,
              totalChildCareSpend = 2400.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          )
        )
      }

    "return output children for 2 children where both children are qualifying" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
        val children = List(
          Child(
            id = 2,
            name = Some("Child 1"),
            childcareCost = BigDecimal(200.00),
            qualifying = true,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = true, severelyDisabled = false)
          ),
          Child(
            id = 3,
            name = Some("Child 2"),
            childcareCost = BigDecimal(800.00),
            qualifying = true,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = false, severelyDisabled = false)
          )
        )
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = children)
        val result = TFCCalculator.calculator.getOutputChildren(tfcPeriod)

        result shouldBe List(
          OutputChild(
            id = 2,
            name = Some("Child 1"),
            childCareCost =  BigDecimal(200.00),
            childContribution = Contribution(
              parent = 600.00,
              government = 0.00,
              totalChildCareSpend = 600.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          ),
          OutputChild(
            id = 3,
            name = Some("Child 2"),
            childCareCost =  BigDecimal(800.00),
            childContribution = Contribution(
              parent = 2400.00,
              government = 0.00,
              totalChildCareSpend = 2400.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          )
        )
      }

    "return output children for 2 children where both children are not qualifying" in
      {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
        val fromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
        val untilDate = LocalDate.parse("2016-05-21T18:46:17", formatter)
        val children = List(
          Child(
            id = 2,
            name = Some("Child 1"),
            childcareCost = BigDecimal(200.00),
            qualifying = false,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = true, severelyDisabled = false)
          ),
          Child(
            id = 3,
            name = Some("Child 2"),
            childcareCost = BigDecimal(800.00),
            qualifying = false,
            from = Some(fromDate),
            until  = Some(untilDate),
            disability = Disability(disabled = false, severelyDisabled = false)
          )
        )
        val tfcPeriod = models.input.tfc.TFCPeriod(from = fromDate, until = untilDate, periodEligibility = false, children = children)
        val result = TFCCalculator.calculator.getOutputChildren(tfcPeriod)

        result shouldBe List(
          OutputChild(
            id = 2,
            name = Some("Child 1"),
            childCareCost =  BigDecimal(200.00),
            childContribution = Contribution(
              parent = 600.00,
              government = 0.00,
              totalChildCareSpend = 600.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          ),
          OutputChild(
            id = 3,
            name = Some("Child 2"),
            childCareCost =  BigDecimal(800.00),
            childContribution = Contribution(
              parent = 2400.00,
              government = 0.00,
              totalChildCareSpend = 2400.00),
            timeToMaximizeTopUp = 0,
            failures = List()
          )
        )
      }

    "Calculate the Total Period contributions for 1 child" in {
      val child = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val result = TFCCalculator.calculator.getPeriodContribution(List(child))

      result shouldBe Contribution(
        parent = BigDecimal(480.00),
        government = BigDecimal(120.00),
        totalChildCareSpend = BigDecimal(600.00)
      )
    }

    "Calculate the Total Period contributions for 2 children" in {
      val child1 = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val child2 = OutputChild(id = 2, name = Some("Child 2"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())

      val result = TFCCalculator.calculator.getPeriodContribution(List(child1, child2))

      result shouldBe Contribution(
        parent = BigDecimal(960.00),
        government = BigDecimal(240.00),
        totalChildCareSpend = BigDecimal(1200.00)
      )
    }

    "Calculate the Total Period contributions for 3 children" in {
      val child1 = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val child2 = OutputChild(id = 2, name = Some("Child 2"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val child3 = OutputChild(id = 2, name = Some("Child 3"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(20.00), BigDecimal(100.00), BigDecimal(100.00)), timeToMaximizeTopUp = 0, failures = List())

      val result = TFCCalculator.calculator.getPeriodContribution(List(child1, child2, child3))

      result shouldBe Contribution(
        parent = BigDecimal(980.00),
        government = BigDecimal(340.00),
        totalChildCareSpend = BigDecimal(1300.00)
      )
    }
    
    "Calculate the household contributions for 1 period for 1 child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodOneFromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val periodOneUntilDate = LocalDate.parse("2016-08-01T18:46:17", formatter)
      val child = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val tfcPeriod1 = models.output.tfc.TFCPeriod(from = periodOneFromDate, until = periodOneUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))

      val result = TFCCalculator.calculator.getHouseholdContribution(List(tfcPeriod1))

      result shouldBe Contribution(
        parent = BigDecimal(480.00),
        government = BigDecimal(120.00),
        totalChildCareSpend = BigDecimal(600.00)
      )
    }

    "Calculate the household contributions for 2 periods for 1 child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodOneFromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val periodOneUntilDate = LocalDate.parse("2016-08-01T18:46:17", formatter)
      val periodTwoFromDate = LocalDate.parse("2016-08-01T18:46:17", formatter)
      val periodTwoUntilDate = LocalDate.parse("2016-11-01T18:46:17", formatter)
      val child = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val tfcPeriod1 = models.output.tfc.TFCPeriod(from = periodOneFromDate, until = periodOneUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))
      val tfcPeriod2 = models.output.tfc.TFCPeriod(from = periodTwoFromDate, until = periodTwoUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))

      val result = TFCCalculator.calculator.getHouseholdContribution(List(tfcPeriod1, tfcPeriod2))

      result shouldBe Contribution(
        parent = BigDecimal(960.00),
        government = BigDecimal(240.00),
        totalChildCareSpend = BigDecimal(1200.00)
      )
    }

    "Calculate the household contributions for 3 periods for 1 child" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodOneFromDate = LocalDate.parse("2016-05-01T18:46:17", formatter)
      val periodOneUntilDate = LocalDate.parse("2016-08-01T18:46:17", formatter)
      val periodTwoFromDate = LocalDate.parse("2016-08-01T18:46:17", formatter)
      val periodTwoUntilDate = LocalDate.parse("2016-11-01T18:46:17", formatter)
      val periodThreeFromDate = LocalDate.parse("2016-11-01T18:46:17", formatter)
      val periodThreeUntilDate = LocalDate.parse("2017-02-01T18:46:17", formatter)
      val child = OutputChild(id = 2, name = Some("Child 1"), childCareCost = BigDecimal(200.00), childContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), timeToMaximizeTopUp = 0, failures = List())
      val tfcPeriod1 = models.output.tfc.TFCPeriod(from = periodOneFromDate, until = periodOneUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))
      val tfcPeriod2 = models.output.tfc.TFCPeriod(from = periodTwoFromDate, until = periodTwoUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))
      val tfcPeriod3 = models.output.tfc.TFCPeriod(from = periodTwoFromDate, until = periodTwoUntilDate, periodContribution = Contribution(BigDecimal(480.00), BigDecimal(120.00), BigDecimal(600.00)), children = List(child))

      val result = TFCCalculator.calculator.getHouseholdContribution(List(tfcPeriod1, tfcPeriod2, tfcPeriod3))

      result shouldBe Contribution(
        parent = BigDecimal(1440.00),
        government = BigDecimal(360.00),
        totalChildCareSpend = BigDecimal(1800.00)
      )
    }
  }
}
