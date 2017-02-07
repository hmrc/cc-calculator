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

import java.lang.Short

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.APIModels.Request
import models.input.tfc.{Child, Disability, TFCEligibility, TFCPeriod}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, CCJsonLogger}

/**
 * Created by roma on 24/12/15.
 */
class TFCEligibilitySpec extends UnitSpec with FakeCCCalculatorApplication with CCJsonLogger{

  "TFC Input JSON" should {
    "read a valid JSON input and convert to a specific type" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tfc/input/calculator_input_test.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[Request]
      result match {
        case JsSuccess(x, _) => {
          x shouldBe a[Request]
          x.payload should not be null

          x.payload.eligibility.tfc.get.from shouldBe a[LocalDate]
          x.payload.eligibility.tfc.get.until shouldBe a[LocalDate]
          x.payload.eligibility.tfc.get.householdEligibility.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tfc.get.periods.head shouldBe a[TFCPeriod]

          //TFC model
          x.payload.eligibility.tfc.get.periods.head.from shouldBe a[LocalDate]
          x.payload.eligibility.tfc.get.periods.head.until shouldBe a[LocalDate]
          x.payload.eligibility.tfc.get.periods.head.children.head shouldBe a[Child]

          //Child model
          x.payload.eligibility.tfc.get.periods.head.children.head.id shouldBe a[Short]
          x.payload.eligibility.tfc.get.periods.head.children.head.name.isInstanceOf[Option[String]] shouldBe true
          x.payload.eligibility.tfc.get.periods.head.children.head.qualifying.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tfc.get.periods.head.children.head.from.isInstanceOf[Option[LocalDate]] shouldBe true
          x.payload.eligibility.tfc.get.periods.head.children.head.until.isInstanceOf[Option[LocalDate]] shouldBe true
          x.payload.eligibility.tfc.get.periods.head.children.head.childcareCost shouldBe a[BigDecimal]
          x.payload.eligibility.tfc.get.periods.head.children.head.disability shouldBe a[Disability]

          //Child Disability model
          x.payload.eligibility.tfc.get.periods.head.children.head.disability.disabled.isInstanceOf[Boolean] shouldBe true
          x.payload.eligibility.tfc.get.periods.head.children.head.disability.severelyDisabled.isInstanceOf[Boolean] shouldBe true
        }
        case _ => throw new Exception
      }
    }
  }

  "TFCEligibility" should {

    "return household eligibility" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse ("2016-06-01T18:46:17",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31T18:46:17",formatter)
      val child = Child(id =1,name = None ,qualifying = true, from = Some(periodStart), until = Some(periodEnd),childcareCost= BigDecimal(18000), disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = periodStart, until = periodEnd, periodEligibility = true, List(child))

      val tfcEligibility = TFCEligibility(
      from = periodStart,
      until = periodEnd,
      householdEligibility = true,
      periods = List(tfcPeriod)
      )
      tfcEligibility.householdEligibility shouldBe true
    }

    "return period eligibility" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse ("2016-06-01T18:46:17",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31T18:46:17",formatter)
      val child = Child(id =1,name = None ,qualifying = true, from = Some(periodStart), until = Some(periodEnd),childcareCost= BigDecimal(18000), disability = Disability(disabled = false, severelyDisabled = false))
      val tfcPeriod = TFCPeriod(from = periodStart, until = periodEnd, periodEligibility = true, List(child))

      val tfcEligibility = TFCEligibility(
        from = periodStart,
        until = periodEnd,
        householdEligibility = true,
        periods = List(tfcPeriod)
      )
      tfcEligibility.periods.head.periodEligibility shouldBe true
    }

    "(TY 2016/2017) return the correct config childcare amount for the tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse("2016-05-06T18:46:17", formatter)
      val periodEnd = LocalDate.parse("2017-04-05T18:46:17", formatter)
      val period = models.input.tfc.TFCPeriod(from = periodStart, until = periodEnd, children = List(), periodEligibility = true)
      period.configRule.maxEligibleChildcareAmount shouldBe 2500
    }

    "(TY 2017/2018) return the correct config government contribution for the tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse("2017-05-06T18:46:17", formatter)
      val periodEnd = LocalDate.parse("2018-04-05T18:46:17", formatter)
      val period = models.input.tfc.TFCPeriod(from = periodStart, until = periodEnd, children = List(), periodEligibility = true)
      period.configRule.maxGovtContribution shouldBe 500
    }

    "(TY 2018/2019) return the correct config for the current tax year (the tax year the period falls into)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse("2018-05-06T18:46:17", formatter)
      val periodEnd = LocalDate.parse("2019-04-05T18:46:17", formatter)
      val period = models.input.tfc.TFCPeriod(from = periodStart, until = periodEnd, children = List(), periodEligibility = true)
      try {
        period.configRule should not be a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

  }

  "TFC Input Child" should {

    "return None for child's name not defined" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
      val periodStart = LocalDate.parse ("2016-06-01T18:46:17",formatter)
      val periodEnd = LocalDate.parse ("2016-08-31T18:46:17",formatter)
      val child = Child(id =1,name = None ,qualifying = true, from = Some(periodStart), until = Some(periodEnd),childcareCost= BigDecimal(18000), disability = Disability(disabled = false, severelyDisabled = false))
      child.name shouldBe None
    }

    "return child disabled status for not disabled & not severely disabled child" in {
      val child = Child(id =1,name = None ,qualifying = true, from = null, until = null, childcareCost= BigDecimal(18000), disability = Disability(disabled = false, severelyDisabled = false))
      child.getChildDisability shouldBe false
      child.getChildSevereDisability shouldBe false
    }

    "return child disabled status for disabled & severely disabled child" in {
      val child = Child(id =1,name = None ,qualifying = true, from = null, until = null, childcareCost= BigDecimal(18000), disability = Disability(disabled = true, severelyDisabled = true))
      child.getChildDisability shouldBe true
      child.getChildSevereDisability shouldBe true
    }

    "return child disabled status for not disabled & severely disabled child" in {
      val child = Child(id =1,name = None ,qualifying = true, from = null, until = null, childcareCost= BigDecimal(18000), disability = Disability(disabled = false, severelyDisabled = true))
      child.getChildDisability shouldBe true
      child.getChildSevereDisability shouldBe true
    }

    "return child disabled status for disabled child" in {
      val child = Child(id =1,name = None ,qualifying = true, from = null, until = null, childcareCost= BigDecimal(18000), disability = Disability(disabled = true, severelyDisabled = false))
      child.getChildDisability shouldBe true
      child.getChildSevereDisability shouldBe false
    }

  }
}