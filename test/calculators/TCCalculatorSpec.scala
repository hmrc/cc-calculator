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

import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader
import models.input.tc._
import models.output.tc.{Element, Elements}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils.{FakeCCCalculatorApplication, Periods, TCConfig}
import scala.collection.immutable.Nil
import scala.concurrent.ExecutionContext.Implicits.global

class TCCalculatorSpec extends UnitSpec with FakeCCCalculatorApplication with org.scalatest.PrivateMethodTester {

  val tcCalculator = new TCCalculator { }

  "tcCalculator" should {

    "round up for double numbers if less than 5" in {
      val cost: BigDecimal = 12.241212342
      val result: BigDecimal = tcCalculator.roundup(cost)
      result shouldBe 12.25
    }

    "round up for double numbers if less than 5 (.00001)" in {
      val cost: BigDecimal = 12.00001
      val result: BigDecimal = tcCalculator.roundup(cost)
      result shouldBe 12.01
    }

    "round up for double numbers if more than 5 (.9599)" in {
      val cost: BigDecimal = 12.9599231531231
      val result: BigDecimal = tcCalculator.roundup(cost)
      result shouldBe 12.96
    }

    "round up for double numbers if less than 5 (.4444)" in {
      val cost: BigDecimal = 12.4444000000001
      val result: BigDecimal = tcCalculator.roundup(cost)
      result shouldBe 12.45
    }

    "not round up for double numbers if all digits after decimal point are 0" in {
      val cost: BigDecimal = 12.9800
      val result: BigDecimal = tcCalculator.roundup(cost)
      result shouldBe 12.98
    }

    "pro-tata an amount of money between two dates (after is before until date)" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-05-2016", formatter)
      val toDate = LocalDate.parse("21-04-2016", formatter)
      //String representation in order to test the whole value
      val result = tcCalculator.amountForDateRange(BigDecimal(0.00), Periods.Weekly, fromDate, toDate)
      result shouldBe BigDecimal(0.00)
    }

    "(weekly) pro-rata an amount of money between two dates (not rounded and not truncated)" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      //String representation in order to test the whole value
      val result: String = tcCalculator.amountForDateRange(cost, Periods.Weekly, fromDate, toDate).toString()
      result shouldBe "2849.40"
    }

    "(Monthly) pro-rata an amount of money between two dates (not rounded and not truncated)" in {
      val cost: BigDecimal = 100.99
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      //String representation in order to test the whole value
      val result: String = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate).toString()
      result shouldBe "66.40"
    }

    "(Yearly) pro-rata an amount of money between two dates (not rounded and not truncated)" in {
      val cost: BigDecimal = 9999.01
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      //String representation in order to test the whole value
      val result: String = tcCalculator.amountForDateRange(cost, Periods.Yearly, fromDate, toDate).toString()
      result shouldBe "547.80"
    }

    "(weekly) pro-rata an amount of money between two dates (not rounded and truncated)" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Weekly, fromDate, toDate)
      result shouldBe 2849.40
    }

    "(monthly) pro-rata an amount of money between two dates (not rounded and truncated)" in {
      val cost: BigDecimal = 100.99
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate)
      result shouldBe 66.400
    }

    "(yearly) pro-rata an amount of money between two dates (not rounded and truncated)" in {
      val cost: BigDecimal = 9999.01
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Yearly, fromDate, toDate)
      result shouldBe 547.80
    }

    "(weekly) pro-rata an amount of money between two dates (rounded and truncated)" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Weekly, fromDate, toDate)
      result shouldBe 2849.40
    }

    "(monthly) pro-rota an amount of money between two dates (rounded and truncated)" in {
      val cost: BigDecimal = 100.99
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate)
      result shouldBe 66.40
    }

    "(yearly) pro-rata an amount of money between two dates (rounded and truncated)" in {
      val cost: BigDecimal = 9999.01
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Yearly, fromDate, toDate)
      result shouldBe 547.80
    }

    "(weekly) pro-rata an amount of money between two dates (rounded and not truncated)" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Weekly, fromDate, toDate)
      result shouldBe 2849.40
    }

    "(monthly) pro-rata an amount of money between two dates (rounded and not truncated)" in {
      val cost: BigDecimal = 100.99
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate)
      result shouldBe 66.40
    }

    "(yearly) pro-rata an amount of money between two dates (rounded and not truncated)" in {
      val cost: BigDecimal = 9999.01
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Yearly, fromDate, toDate)
      result shouldBe 547.80
    }

    "(monthly) pro-rata an amount of money between two dates" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate)
      result shouldBe 657.60
    }

    "(yearly) pro-rata an amount of money between two dates" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2017-05-01", formatter)
      val toDate = LocalDate.parse("2017-05-21", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Yearly, fromDate, toDate)
      result shouldBe 54.80
    }

    "(monthly) pro-rata an amount of money between two dates spanning two years" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2016-12-12", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)
      val result: BigDecimal = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate)
      result shouldBe 3781.20
    }

    "(monthly) pro-rata an amount of money between two dates spanning two years (truncated, but not rounded)" in {
      val cost: BigDecimal = 1000.00
      val fromDate = LocalDate.parse("2016-12-12", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)
      //String representation in order to test the whole value
      val result: String = tcCalculator.amountForDateRange(cost, Periods.Monthly, fromDate, toDate).toString()
      result shouldBe "3781.20"
    }

    "Determine earnings amount per period" in {
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val untilDate = LocalDate.parse("2017-04-06", formatter)
      val period = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val income = BigDecimal(17000)
      val thresholdIncome = BigDecimal(4000)
      val result = tcCalculator.earningsAmountToTaperForPeriod(income, thresholdIncome, period)
      result shouldBe BigDecimal(5330.00)
    }

    "Determine net amount per element per period (taper amount is larger than element's max amount)" in {
      val taperAmount = BigDecimal(17000)
      val maximumAmount = BigDecimal(4000)
      val result = tcCalculator.netAmountPerElementPerPeriod(taperAmount, maximumAmount)
      result shouldBe BigDecimal(0.00)
    }

    "Determine net amount per element per period (taper amount is lower than element's max amount)" in {
      val taperAmount = BigDecimal(200)
      val maximumAmount = BigDecimal(17000)
      val result = tcCalculator.netAmountPerElementPerPeriod(taperAmount, maximumAmount)
      result shouldBe BigDecimal(16800.00)
    }

    "Determine percentage of an amount" in {
      val amount = BigDecimal(10000.00)
      val percentage = 10
      val result = tcCalculator.getPercentOfAmount(amount, percentage)
      result shouldBe BigDecimal(1000.00)
    }

    "Return an instance of TCCalculator" in {
      val calc = tcCalculator
      calc.isInstanceOf[TCCalculator] shouldBe true
    }

    "(qualifying) determine if get basic element and the amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val basicElement = tcCalculator.basicElementForPeriod(period.head)
      basicElement shouldBe BigDecimal(1025.67)
    }

    "(non qualifying) determine if get basic element and the amount for the period" in {
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)
      val period = models.input.tc.TCPeriod(
        from = fromDate,
        until = toDate,
        householdElements = TCHouseholdElements(
          basic = false,
          hours30 = false,
          childcare = false,
          loneParent = false,
          secondParent = false,
          family = false
        ),
        claimants = List(),
        children = List()
      )
      val getsBasicElement = tcCalculator.basicElementForPeriod(period)
      getsBasicElement shouldBe BigDecimal(0.00)
    }

    "determine if get 30 hours element and the amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_7.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val hours30Element = tcCalculator.hours30ElementForPeriod(period.head)
      hours30Element shouldBe BigDecimal(424.02)
    }

    "determine if get disabled worker element and the amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_13.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val claimant = period.head.claimants.head
      val workerDisabiltyElement = tcCalculator.disabledWorkerElementForPeriod(period.head, claimant)
      workerDisabiltyElement shouldBe BigDecimal(1554.74)
    }

    "determine if get severely disabled worker element and the amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_19.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val claimant = period.head.claimants.head
      val severelyDisabledWorkerElement = tcCalculator.severelyDisabledWorkerElementForPeriod(period.head, claimant)
      severelyDisabledWorkerElement shouldBe BigDecimal(666.59)
    }

    "determine if get lone parent element and amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val loneParentElement = tcCalculator.loneParentElementForPeriod(period.head)
      loneParentElement shouldBe BigDecimal(1052.41)
    }

    "determine if get second adult element and amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val coupleElement = tcCalculator.secondAdultElementForPeriod(period.head)
      coupleElement shouldBe BigDecimal(1052.41)
    }

    "(qualifying) determine if get family element for period and amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val familyElement = tcCalculator.maxFamilyElementForPeriod(period.head)
      familyElement shouldBe BigDecimal(284.59)
    }

    "(non - qualifying) determine if get family element for period and amount for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods.tail.head
      val familyElement = tcCalculator.maxFamilyElementForPeriod(period)
      familyElement shouldBe BigDecimal(0.00)
    }

    "(qualifying) Determine if child gets the child basic element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childElement = tcCalculator.childOrYoungAdultBasicElementForPeriod(period.head, period.head.children.head)
      childElement shouldBe BigDecimal(1455.42)
    }

    "(non-qualifying) Determine if child gets the child basic element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_51.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods.tail.head
      val childElement = tcCalculator.childOrYoungAdultBasicElementForPeriod(period, period.children.head)
      childElement shouldBe BigDecimal(0.00)
    }

    "(qualifying) Determine if child gets disability element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_11.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childDisabiltyElement = tcCalculator.childOrYoungAdultDisabilityElementForPeriod(period.head, period.head.children.head)
      childDisabiltyElement shouldBe BigDecimal(1642.60)
    }

    "(non - qualifying) Determine if child gets disability element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childDisabiltyElement = tcCalculator.childOrYoungAdultDisabilityElementForPeriod(period.head, period.head.children.head)
      childDisabiltyElement shouldBe BigDecimal(0.00)
    }

    "(qualifying) Determine if child gets severe disability element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_11.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childSevereDisabiltyElement = tcCalculator.childOrYoungAdultSevereDisabilityElementForPeriod(period.head, period.head.children.head)
      childSevereDisabiltyElement shouldBe BigDecimal(666.59)
    }

    "(non-qualifying) Determine if child gets severe disability element for the period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_21.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childSevereDisabiltyElement = tcCalculator.childOrYoungAdultSevereDisabilityElementForPeriod(period.head, period.head.children.head)
      childSevereDisabiltyElement shouldBe BigDecimal(0.00)
    }

    "(qualifying) determine child element(s) (as a total) for multiple children" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_44.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val childElement = tcCalculator.maxChildElementForPeriod(period.head)
      childElement shouldBe BigDecimal(8318.05)
    }

    "(one child qualifying, one not qualifying)(child + child) determine child element(s) (as a total) for multiple children" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val period1ChildElement = tcCalculator.maxChildElementForPeriod(period.head)
      period1ChildElement shouldBe BigDecimal(1232.72)

      // second period
      val period2ChildElement = tcCalculator.maxChildElementForPeriod(period.tail.head)
      period2ChildElement shouldBe BigDecimal(0.00)
    }

    "(qualifying)(young adult + child) determine child element(s) (as a total) for multiple children" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_50.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val period1ChildElement = tcCalculator.maxChildElementForPeriod(period.head)
      period1ChildElement shouldBe BigDecimal(1158.24)

      // second period
      val period2ChildElement = tcCalculator.maxChildElementForPeriod(period.tail.head)
      period2ChildElement shouldBe BigDecimal(876.30)
    }

    "(no children) return BigDecimal(0.00) when checking weekly threshold spend for children" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(),
        children = List())

      val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
      val result = tcCalculator invokePrivate decoratedChildCareThreshold(inputPeriod)
      result shouldBe BigDecimal(0.00)
    }

    "(1 child) return 1 child threshold when checking weekly threshold spend for children" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val child = TCChild(childcareCost = BigDecimal(2000.00), childcareCostPeriod = Periods.Monthly,
        childElements = TCChildElements(childcare = true))

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(),
        children = List(child))

      val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
      val result = tcCalculator invokePrivate decoratedChildCareThreshold(inputPeriod)
      result shouldBe BigDecimal(175.00)
    }

    "(2 children) return 1 child threshold when 1 child has no childcare cost when checking weekly threshold spend for children" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val child1 = TCChild(childcareCost = BigDecimal(2000.00), childcareCostPeriod = Periods.Monthly,
        childElements = TCChildElements(childcare = true))
      val child2 = TCChild(childcareCost = BigDecimal(0.00), childcareCostPeriod = Periods.Monthly,
        childElements = TCChildElements(childcare = true))

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(),
        children = List(child1, child2))

      val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
      val result = tcCalculator invokePrivate decoratedChildCareThreshold(inputPeriod)
      result shouldBe BigDecimal(175.00)
    }

    "(5 children) return multiple child threshold when checking weekly threshold spend for children" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val child1 = TCChild(childcareCost = BigDecimal(500.00), childcareCostPeriod = Periods.Monthly,
        childElements = TCChildElements(childcare = true))
      val child2 = TCChild(childcareCost = BigDecimal(2000.00), childcareCostPeriod = Periods.Monthly, childElements = TCChildElements())
      val child3 = TCChild(childcareCost = BigDecimal(300.00), childcareCostPeriod = Periods.Monthly, childElements = TCChildElements())
      val child4 = TCChild(childcareCost = BigDecimal(200.00), childcareCostPeriod = Periods.Monthly,
        childElements = TCChildElements(childcare = true))
      val child5 = TCChild(childcareCost = BigDecimal(100.00), childcareCostPeriod = Periods.Monthly, childElements = TCChildElements())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(),
        children = List(child1, child2, child3, child4, child5))

      val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
      val result = tcCalculator invokePrivate decoratedChildCareThreshold(inputPeriod)
      result shouldBe BigDecimal(300.00)
    }

    "(all not qualifying) determine child element(s) (as a total) for multiple children" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val period1ChildElement = tcCalculator.maxChildElementForPeriod(period.head)
      period1ChildElement shouldBe BigDecimal(1232.72)
      // second period
      val period2ChildElement = tcCalculator.maxChildElementForPeriod(period.tail.head)
      period2ChildElement shouldBe BigDecimal(0.00)
    }

    "(no children) determine the child element(s) (as a total) for no children" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_57.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val period1ChildElement = tcCalculator.maxChildElementForPeriod(period.head)
      period1ChildElement shouldBe BigDecimal(0.00)
    }

    "(claimant with partner both qualifying) determine wtc work element(s) (as a total) for multiple claimants" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_18.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val workElement = tcCalculator.maxWorkElementForPeriod(period.head)
      workElement shouldBe BigDecimal(5187.56)
    }

    "(claimant qualifying without partner) determine wtc work element (as a total) for single claimant" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_27.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val workElement = tcCalculator.maxWorkElementForPeriod(period.head)
      workElement shouldBe BigDecimal(4056.84)
    }

    "(claimant with partner both qualifying) determine wtc work element(s) (as a total) for multiple claimants with severe disability" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_32.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val workElement = tcCalculator.maxWorkElementForPeriod(period.head)
      workElement shouldBe BigDecimal(6944.76)
    }

    "(claimant without partner without children) determine wtc work element(s) (as a total) for single claimant (Not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_54.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val workElement = tcCalculator.maxWorkElementForPeriod(period.head)
      workElement shouldBe BigDecimal(1025.67)
    }

    "(claimant with partner without children) determine wtc work element(s) (as a total) for multiple claimants (Not applicable for our journey)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_55.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val workElement = tcCalculator.maxWorkElementForPeriod(period.head)
      workElement shouldBe BigDecimal(2580.41)
    }

    "Determine WTC childcare element when there is only one child (not exceeding the element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(878.41)
    }

    "Determine WTC childcare element when there is only one child (exceeding the element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_2.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(3333.14)
    }

    "Determine WTC childcare element when there are 2 children (not exceeding childcare element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_39.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(5687.60)
    }

    "Determine WTC childcare element when there are 2 children (edge case -> 300p/w)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_38.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(5700.97)
    }

    "Determine WTC childcare element when there are 2 children (exceeding childcare element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_37.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(5714.34)
    }

    "Determine WTC childcare element when there are 3 children (not exceeding the element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_43.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(2637.90)
    }

    "Determine WTC childcare element when there are 3 children (exceeding the element limit)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_44.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period.head)
      wtcChildcareElement shouldBe BigDecimal(5714.34)
    }

    "Determine award period start and end dates when there is only one period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)
      val award = tcCalculator.award(result.get)
      award.from shouldBe fromDate
      award.until shouldBe toDate
    }

    "Determine award period start and end dates when there is more than one period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)
      val award = tcCalculator.award(result.get)
      award.from shouldBe fromDate
      award.until shouldBe toDate
    }

    "(2016/2017) Determine wtc income threshold for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val threshold = tcCalculator.wtcIncomeThresholdForPeriod(period.head)
      threshold shouldBe BigDecimal(3359.69)
    }

    "(2016/2017) Determine ctc income threshold for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val threshold = tcCalculator.ctcIncomeThresholdForPeriod(period.head)
      threshold shouldBe BigDecimal(8426.92)
    }

    "(2017/2018) Determine wtc income threshold for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val threshold = tcCalculator.wtcIncomeThresholdForPeriod(period.head)
      threshold shouldBe BigDecimal(3359.69)
    }

    "(2017/2018) Determine ctc income threshold for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val threshold = tcCalculator.ctcIncomeThresholdForPeriod(period.head)
      threshold shouldBe BigDecimal(8426.92)
    }

    "Determine single claimant income for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val income = 17000 
      val threshold = tcCalculator.incomeForPeriod(income, period.head)
      threshold shouldBe BigDecimal(8896.78)
    }

    "Determine multiple claimant income for a period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_32.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val period = result.get.taxYears.head.periods
      val income = 34000
      val threshold = tcCalculator.incomeForPeriod(income, period.head)
      threshold shouldBe BigDecimal(17791.65)
    }

    "(requires tapering) Determine if tapering of elements is required" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-12-2016", formatter)
      val threshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val income = BigDecimal(6420.01)
      val result = tcCalculator.isTaperingRequiredForElements(income, threshold)
      result shouldBe true
    }

    "(does not require tapering - income same as threshold) Determine if tapering of elements is required" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-12-2016", formatter)
      val threshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val income = BigDecimal(6420.00)
      val result = tcCalculator.isTaperingRequiredForElements(income, threshold)
      result shouldBe false
    }

    "(does not require tapering - income less) Determine if tapering of elements is required" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-12-2016", formatter)
      val threshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val income = BigDecimal(200.00)
      val result = tcCalculator.isTaperingRequiredForElements(income, threshold)
      result shouldBe false
    }

    "Determine which amount is higher (one amount is higher)" in {
      val amount = BigDecimal(100.00)
      val higherAmount = BigDecimal(100.0001)
      val result = tcCalculator.getHigherAmount(amount, higherAmount)
      result shouldBe higherAmount
    }

    "Determine which amount is higher (both amounts are the same)" in {
      val amount = BigDecimal(100.00)
      val higherAmount = BigDecimal(100.00)
      val result = tcCalculator.getHigherAmount(amount, higherAmount)
      result shouldBe higherAmount
      higherAmount shouldBe amount
    }

    "(does not require tapering - income less than threshold) Determine the net amounts of the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 5000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(4737.70)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(748.80)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(5504.20)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(547.50)
    }

    "(tapers all amounts fully) Determine the net amounts of the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
    }

    "(no tapering required, single claimant claiming social security benefit) Determine the net amounts of a the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = false, claimantElements = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimantClaimingSocialSecurityBenefit), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(4737.70)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(748.80)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(5504.20)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(547.50)
    }

    "(tapering required, single claimant not claiming social security benefit) Determine the net amounts of a the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = false, isPartner = false, claimantElements = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimantClaimingSocialSecurityBenefit), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
    }

    "(no tapering required, joint claimants, one claimant claiming social security benefit) Determine the net amounts of a the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val claimant = models.input.tc.TCClaimant(qualifying = true, isPartner = false, claimantElements = TCDisability())
      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = true, claimantElements = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimant, claimantClaimingSocialSecurityBenefit), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(4737.70)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(748.80)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(5504.20)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(547.50)
    }

    "(no tapering required, joint claimants, both claimants claiming social security benefit) Determine the net amounts of a the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val claimantClaimingSocialSecurityBenefit1 = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = false, claimantElements = TCDisability())
      val claimantClaimingSocialSecurityBenefit2 = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = true, claimantElements = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimantClaimingSocialSecurityBenefit1, claimantClaimingSocialSecurityBenefit2), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(4737.70)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(748.80)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(5504.20)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(547.50)
    }

    "(tapering required, joint claimants, both not claiming social security benefit) Determine the net amounts of a the period" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 100000.00
      val wtcThreshold = TCConfig.getConfig(fromDate).thresholds.wtcIncomeThreshold
      val ctcThreshold = TCConfig.getConfig(fromDate).thresholds.ctcIncomeThreshold
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val claimant1 = models.input.tc.TCClaimant(qualifying = true, isPartner = false, claimantElements = TCDisability())
      val claimant2 = models.input.tc.TCClaimant(qualifying = true, isPartner = true, claimantElements = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimant1, claimant2), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
    }

    "generate the maximum amounts for a period model" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]

      result match {
        case JsSuccess(x, _) =>
          // period 1
          val p = x.taxYears.head.periods.head
          val setup = tcCalculator.generateMaximumAmountsForPeriod(p)
          setup.elements.wtcWorkElement.maximumAmount shouldBe BigDecimal(2078.08)
          setup.elements.wtcChildcareElement.maximumAmount shouldBe BigDecimal(878.41)
          setup.elements.ctcIndividualElement.maximumAmount shouldBe BigDecimal(1455.42)
          setup.elements.ctcFamilyElement.maximumAmount shouldBe BigDecimal(284.59)
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Generate the maximum amounts for a period model (no children, get just basic element)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_54.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]

      val p = result.get.taxYears.head.periods.head
      val setup = tcCalculator.generateMaximumAmountsForPeriod(p)
      setup.elements.wtcWorkElement.maximumAmount shouldBe BigDecimal(1025.67)
      setup.elements.wtcChildcareElement.maximumAmount shouldBe BigDecimal(0.00)
      setup.elements.ctcIndividualElement.maximumAmount shouldBe BigDecimal(0.00)
      setup.elements.ctcFamilyElement.maximumAmount shouldBe BigDecimal(0.00)
    }

    "(no tapering required) Taper the first element (WTC element)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_54.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val p = result.get.taxYears.head.periods.head
      val income = 17000 //x.taxYears.head.houseHoldIncome
      val setup = tcCalculator.generateMaximumAmountsForPeriod(p)
      val i = tcCalculator.incomeForPeriod(income, p)
      val incomeThreshold = tcCalculator.wtcIncomeThresholdForPeriod(period = p)
  
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val untilDate = LocalDate.parse("2017-04-06", formatter)
      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())
  
      val output = models.output.tc.Period(
        from = fromDate,
        until = untilDate,
        elements = Elements(
          wtcWorkElement = Element(
            maximumAmount = 1025.67,
            taperAmount = 1025.67,
            netAmount = 0.00
          ),
          wtcChildcareElement = Element(
            maximumAmount = 0.00,
            taperAmount = 0.00,
            netAmount = 0.00
          ),
          ctcIndividualElement = Element(
            maximumAmount = 0.00,
            taperAmount = 0.00,
            netAmount = 0.00
          ),
          ctcFamilyElement = Element(
            maximumAmount = 0.00,
            taperAmount = 0.00,
            netAmount = 0.00
          )
        )
      )
  
      val taperedPeriodModel = tcCalculator.taperFirstElement(period = setup, inputPeriod = inputPeriod, income = i, wtcIncomeThreshold = incomeThreshold)
      taperedPeriodModel shouldBe output
    }

    "(full tapering) taper the first element (WTC work element)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }


      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFirstAmount = tcCalculator.taperFirstElement(period, inputPeriod, income, wtcIncomeThreshold)
      taperFirstAmount.elements.wtcWorkElement.netAmount shouldBe BigDecimal(0.00)
      taperFirstAmount.elements.wtcWorkElement.taperAmount shouldBe BigDecimal(4737.70)
    }

    "Populate the output model's net amounts to be maximum amounts when no tapering is required" in {
      val formatter = DateTimeFormat.forPattern("dd-MM-yyyy")
      val fromDate = LocalDate.parse("01-05-2016", formatter)
      val toDate = LocalDate.parse("21-05-2017", formatter)
      val totalMaximumAmount = BigDecimal(400.00)
      val inputedModel = models.output.tc.Period(
        from = fromDate,
        until = toDate,
        elements = Elements(
          wtcWorkElement = Element(
            maximumAmount = BigDecimal(100.00)
          ),
          wtcChildcareElement = Element(
            maximumAmount = BigDecimal(100.00)
          ),
          ctcIndividualElement = Element(
            maximumAmount = BigDecimal(100.00)
          ),
          ctcFamilyElement = Element(
            maximumAmount = BigDecimal(100.00)
          )
        )
      )
      val decoratedOutputModel = PrivateMethod[models.output.tc.Period]('getPeriodAmount)
      val result = tcCalculator invokePrivate decoratedOutputModel(inputedModel, totalMaximumAmount, true)
      result.elements.wtcWorkElement.maximumAmount shouldBe BigDecimal(100.00)
      result.elements.wtcChildcareElement.maximumAmount shouldBe BigDecimal(100.00)
      result.elements.ctcIndividualElement.maximumAmount shouldBe BigDecimal(100.00)
      result.elements.ctcFamilyElement.maximumAmount shouldBe BigDecimal(100.00)
    }

    "(partial tapering) Taper the first element (WTC work element)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 5837.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 748.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFirstAmount = tcCalculator.taperFirstElement(period, inputPeriod, income, wtcIncomeThreshold)
      taperFirstAmount.elements.wtcWorkElement.netAmount shouldBe BigDecimal(343.70)
      taperFirstAmount.elements.wtcWorkElement.taperAmount shouldBe BigDecimal(5494.00)
    }

    "determine if tapering second element (WTC childcare element) is required" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 58.00,
              taperAmount = 5494
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperSecondAmount = tcCalculator.taperSecondElement(period, inputPeriod, income, wtcIncomeThreshold)
      taperSecondAmount.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(728.80)
      taperSecondAmount.elements.wtcChildcareElement.taperAmount shouldBe BigDecimal(0.00)
    }

    "(full tapering) taper the second element (WTC childcare element)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperSecondAmount = tcCalculator.taperSecondElement(period, inputPeriod, income, wtcIncomeThreshold)
      taperSecondAmount.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(0.00)
      taperSecondAmount.elements.wtcChildcareElement.taperAmount shouldBe BigDecimal(728.80)
    }

    "(partial tapering) Taper the second element (WTC childcare element)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 828.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperSecondAmount = tcCalculator.taperSecondElement(period, inputPeriod, income, wtcIncomeThreshold)
      taperSecondAmount.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(72.50)
      taperSecondAmount.elements.wtcChildcareElement.taperAmount shouldBe BigDecimal(756.30)
    }

    "determine if tapering third element (CTC Child Element) is required" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 19000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 4000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 72.50,
              taperAmount = 756.30
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(5504.20)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(0.00)
      taperThirdAmount._2 shouldBe false
    }

    "(full tapering) taper the third element (CTC child element) when calculated CTC Threshold is greater than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)
      val income = 21000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 15000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 500.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(500.20)
      taperThirdAmount._2 shouldBe true
    }

    "(full tapering) taper the third element (CTC child element) when calculated CTC Threshold is less than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 21000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 19500
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 500.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(500.20)
      taperThirdAmount._2 shouldBe true
    }

    "(partial tapering) taper the third element (CTC child element) when calculated CTC Threshold is greater than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 21000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 15000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 1200.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(352.7)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(847.50)
      taperThirdAmount._2 shouldBe true
    }

    "(partial tapering) taper the third element (CTC child element) when calculated CTC Threshold is less than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 21000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 19500
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 2500.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(1885.20)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(615.00)
      taperThirdAmount._2 shouldBe true
    }

    "(partial tapering) taper the third element (CTC child element) when calculated CTC Threshold is greater than ctcIncomeThreshold and income" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 12000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 11000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperThirdAmount = tcCalculator.taperThirdElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
      taperThirdAmount._1.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(3400.20)
      taperThirdAmount._1.elements.ctcIndividualElement.taperAmount shouldBe BigDecimal(0.00)
      taperThirdAmount._2 shouldBe true
    }

    "determine if tapering fourth element (CTC family element)is required" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 12000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 11000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, false)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(547.50)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(0.00)
    }

    "(full tapering) taper fourth element (CTC family element) when calculated CTC threshold is greater than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 30000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 11000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, true)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(547.50)
    }

    "(full tapering) taper fourth element (CTC family element) when calculated CTC threshold is lesser than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 30000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 29000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 350.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, true)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(350.50)
    }

    "(patrial tapering) taper fourth element (CTC family element) when calculated CTC threshold is greater than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 30000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 11000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 1300.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, true)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(163.20)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(1137.30)
    }


    "(partial tapering) taper fourth element (CTC family element) when calculated CTC threshold is lesser than ctcIncomeThreshold" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 30000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 29000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 450.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, true)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(40.50)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(410.00)
    }

    "(partial tapering) taper fourth element (CTC family element) when calculated CTC threshold is greater than ctcIncomeThreshold and income" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val income = 25000
      val wtcIncomeThreshold = 5600
      val ctcIncomeThreshold = 11000
      val period = {
        models.output.tc.Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 1300.50
            )
          )
        )
      }

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val taperFourthAmount = tcCalculator.taperFourthElement(period, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold, true)
      taperFourthAmount.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(1300.50)
      taperFourthAmount.elements.ctcFamilyElement.taperAmount shouldBe BigDecimal(0.00)
    }

    "Determine that the list of periods is not populated when the period list is empty" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val untilDate = LocalDate.parse("2017-04-06", formatter)

      val tcEligibility = TCCalculatorInput(
        taxYears = List(
          TCTaxYear(
            from = fromDate,
            until = untilDate,
            previousHouseholdIncome = TCIncome(None, None, None, None, None),
            currentHouseholdIncome = TCIncome(None, None, None, None, None),
            periods = List()
          ))
      )

      val taxYear = tcEligibility.taxYears.head
      val income = 0 //taxYear.houseHoldIncome
      val setup = tcCalculator.getCalculatedPeriods(taxYear, income)

      setup shouldBe Nil
    }

    "Determine that the list of periods is populated when there is one period" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-09-27", formatter)
      val toDate = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          // period 1
          val taxYear = x.taxYears.head
          val income = 17000 // x.taxYears.head.houseHoldIncome
          val setup = tcCalculator.getCalculatedPeriods(taxYear, income)

          setup.head should not be Nil
          setup.head.from shouldBe fromDate
          setup.head.until shouldBe toDate
          setup.head.periodNetAmount shouldBe 2426.29
          setup.head.elements.ctcFamilyElement should not be Nil
          setup.head.elements.wtcWorkElement should not be Nil
          setup.head.elements.ctcIndividualElement should not be Nil
          setup.head.elements.wtcChildcareElement should not be Nil

          setup.tail shouldBe Nil
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine that the list of periods is populated when there are multiple periods" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val firstPeriodTo = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodFrom = LocalDate.parse("2016-12-12", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          // period 1
          val taxYear = x.taxYears.head
          val income = 17000 //x.taxYears.head.houseHoldIncome
          val setup = tcCalculator.getCalculatedPeriods(taxYear, income)

          setup.head should not be Nil
          setup.head.from shouldBe firstPeriodFrom
          setup.head.until shouldBe firstPeriodTo
          setup.head.periodNetAmount shouldBe 1787.75
          setup.head.elements.ctcFamilyElement should not be Nil
          setup.head.elements.wtcWorkElement should not be Nil
          setup.head.elements.ctcIndividualElement should not be Nil
          setup.head.elements.wtcChildcareElement should not be Nil

          setup.tail should not be Nil
          setup.tail.head.from shouldBe secondPeriodFrom
          setup.tail.head.until shouldBe secondPeriodTo
          setup.tail.head.periodNetAmount shouldBe 0.00
          setup.tail.head.elements.ctcFamilyElement should not be Nil
          setup.tail.head.elements.wtcWorkElement should not be Nil
          setup.tail.head.elements.ctcIndividualElement should not be Nil
          setup.tail.head.elements.wtcChildcareElement should not be Nil
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine total award for calculation (one period)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val firstPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.award(x)

          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe firstPeriodTo
          setup.totalAwardAmount shouldBe 2426.29
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine total award for calculation (two periods)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_52.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.award(x)

          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe secondPeriodTo
          setup.totalAwardAmount shouldBe 1787.75
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine calculation amount for the award amount" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.award(x)
          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe secondPeriodTo
          setup.totalAwardAmount shouldBe 2426.29
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine calculation amount for the award amount (Multiple Tax years)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2017-09-27", formatter)
      val lastPeriodTo = LocalDate.parse("2018-02-15", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.award(x)
          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe lastPeriodTo
          setup.totalAwardAmount shouldBe 10307.22
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine advice amount (including just the WTC threshold)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val maxHouseholdAmount = 15000.00
      val wtcThresholdPerPeriod = 6420

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.getAdviceCalculationRounded(maxHouseholdAmount, wtcThresholdPerPeriod, inputPeriod)
      result shouldBe 43020.00
    }

    "Determine advice amount (including just the WTC threshold with decimal values)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val maxHouseholdAmount = 13425.64
      val wtcThresholdPerPeriod = 6420

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.getAdviceCalculationRounded(maxHouseholdAmount, wtcThresholdPerPeriod, inputPeriod)
      result shouldBe 39178.5616
    }

    "Determine advice amount (including just the WTC threshold value as 0)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val maxHouseholdAmount = 0.00
      val wtcThresholdPerPeriod = 6420

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.getAdviceCalculationRounded(maxHouseholdAmount, wtcThresholdPerPeriod, inputPeriod)
      result shouldBe 6420
    }

    "Determine advice amount (including just the WTC threshold having negative value)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val fromDate = LocalDate.parse("2016-05-01", formatter)
      val untilDate = LocalDate.parse("2016-05-21", formatter)

      val maxHouseholdAmount = -9452.12
      val wtcThresholdPerPeriod = 6420

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(), children = List())

      val result = tcCalculator.getAdviceCalculationRounded(maxHouseholdAmount, wtcThresholdPerPeriod, inputPeriod)
      result shouldBe -16643.1728
    }

    "Return advice amount set in the total award model of TCCalculation (Successful)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.incomeAdvice(x)
          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe secondPeriodTo
          setup.totalAwardAmount shouldBe 0.00
          setup.houseHoldAdviceAmount shouldBe 14819.1500
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Return advice amount set in the total award model of TCCalculation (Successful) (Multiple Tax years)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2017-09-27", formatter)
      val lastPeriodTo = LocalDate.parse("2018-02-15", formatter)

      result match {
        case JsSuccess(x, _) =>
          val setup = tcCalculator.incomeAdvice(x)
          setup should not be Nil
          setup.from shouldBe firstPeriodFrom
          setup.until shouldBe lastPeriodTo
          setup.totalAwardAmount shouldBe 0.00
          setup.houseHoldAdviceAmount shouldBe 40944.1676
        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "Determine total Maximum amount for a period (populated model) " in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      val period =
        models.output.tc.Period(
          from = firstPeriodFrom,
          until = secondPeriodTo,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 1300.50
            )
          )
        )
      val totalMaximumAmount = tcCalculator.getTotalMaximumAmountPerPeriod(period)
      totalMaximumAmount shouldBe 10167.20

    }

    "Determine total Maximum amount for a period (empty model)" in {
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      val period = {
        models.output.tc.Period(
          from = firstPeriodFrom,
          until = secondPeriodTo,
          elements = Elements(
            wtcWorkElement = Element(),
            wtcChildcareElement = Element(),
            ctcIndividualElement = Element(),
            ctcFamilyElement = Element()
          )
        )
      }
      val totalMaximumAmount = tcCalculator.getTotalMaximumAmountPerPeriod(period)
      totalMaximumAmount shouldBe 0.00
    }

    "Populate Period model when calculating household advice" in {
      val adviceAmount = BigDecimal(10000.00)
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)
      val period = {
        models.output.tc.Period(
          from = firstPeriodFrom,
          until = secondPeriodTo,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = 4737.70,
              netAmount = 0.00,
              taperAmount = 4737.70
            ),
            wtcChildcareElement = Element(
              maximumAmount = 728.80,
              netAmount = 0.00,
              taperAmount = 728.80
            ),
            ctcIndividualElement = Element(
              maximumAmount = 3400.20,
              netAmount = 0.00
            ),
            ctcFamilyElement = Element(
              maximumAmount = 1300.50
            )
          )
        )
      }
      val decoratedAdvicePeriod = PrivateMethod[models.output.tc.Period]('getPeriodAmount)
      val result = tcCalculator invokePrivate decoratedAdvicePeriod(period, adviceAmount, false)
      result should not be Nil
      result.periodNetAmount shouldBe 0.00
      result.periodAdviceAmount shouldBe 10000.00
    }

    "populate tax years model when there is only one tax year" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val listOfTaxYears = tcCalculator.getCalculatedTaxYears(x)
          listOfTaxYears should not be Nil
          listOfTaxYears.head.from shouldBe firstPeriodFrom
          listOfTaxYears.head.until shouldBe secondPeriodTo
          listOfTaxYears.head.taxYearAwardAmount should not be BigDecimal(0.00)
          listOfTaxYears.head.periods should not be Nil

        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "populate tax years model when there are two tax years (Total Award Calculation)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2017-09-27", formatter)
      val lastPeriodTo = LocalDate.parse("2018-02-15", formatter)

      result match {
        case JsSuccess(x, _) =>
          val listOfTaxYears = tcCalculator.getCalculatedTaxYears(x)
          listOfTaxYears should not be Nil
          listOfTaxYears.head.from shouldBe firstPeriodFrom
          listOfTaxYears.tail.head.until shouldBe lastPeriodTo
          listOfTaxYears.head.taxYearAwardAmount should not be BigDecimal(0.00)
          listOfTaxYears.head.periods should not be Nil

        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "populate tax years model when there is only one tax year (Income Advice Calculation)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2016/scenario_1.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2016-09-27", formatter)
      val secondPeriodTo = LocalDate.parse("2017-04-06", formatter)

      result match {
        case JsSuccess(x, _) =>
          val listOfTaxYears = tcCalculator.getCalculatedTaxYears(x, true)
          listOfTaxYears should not be Nil
          listOfTaxYears.head.from shouldBe firstPeriodFrom
          listOfTaxYears.head.until shouldBe secondPeriodTo
          listOfTaxYears.head.taxYearAwardAmount shouldBe BigDecimal(0.00)
          listOfTaxYears.head.taxYearAdviceAmount should not be BigDecimal(0.00)

          listOfTaxYears.head.periods should not be Nil

        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

    "populate tax years model when there are two tax years (Income Advice Calculation)" in {
      val resource: JsonNode = JsonLoader.fromResource("/json/tc/input/2017/scenario_3.json")
      val json: JsValue = Json.parse(resource.toString)
      val result = json.validate[TCCalculatorInput]
      val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
      val firstPeriodFrom = LocalDate.parse("2017-09-27", formatter)
      val lastPeriodTo = LocalDate.parse("2018-02-15", formatter)

      result match {
        case JsSuccess(x, _) =>
          val listOfTaxYears = tcCalculator.getCalculatedTaxYears(x, true)
          listOfTaxYears should not be Nil
          listOfTaxYears.head.from shouldBe firstPeriodFrom
          listOfTaxYears.tail.head.until shouldBe lastPeriodTo
          listOfTaxYears.head.taxYearAwardAmount shouldBe BigDecimal(0.00)
          listOfTaxYears.head.taxYearAdviceAmount should not be BigDecimal(0.00)

          listOfTaxYears.head.periods should not be Nil

        case JsError(e) => throw new RuntimeException(e.toList.toString)
      }
    }

  }
}
