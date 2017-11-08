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
import models.output.tc.{Period, Element, Elements}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import uk.gov.hmrc.play.test.UnitSpec
import utils._
import scala.collection.immutable.Nil
import scala.concurrent.ExecutionContext.Implicits.global
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach

class TCCalculatorSpec extends UnitSpec with FakeCCCalculatorApplication with MockitoSugar with BeforeAndAfterEach with org.scalatest.PrivateMethodTester {

  val tcCalculator = new TCCalculator {
    override val tcConfig: TCConfig = mock[TCConfig]
  }
  val mockTCTaxYearConfig: TCTaxYearConfig = mock[TCTaxYearConfig]
  val mockThresholds: Thresholds = mock[Thresholds]
  val mockWTC = mock[WTC]
  val mockCTC = mock[CTC]

  when(
    tcCalculator.tcConfig.getConfig(any[LocalDate]())
  ).thenReturn(
    mockTCTaxYearConfig
  )

  when(
    mockTCTaxYearConfig.otherIncomeAdjustment
  ).thenReturn(
    300
  )

  when(
    mockTCTaxYearConfig.currentIncomeRiseDifferenceAmount
  ).thenReturn(
    2500
  )

  when(
    mockTCTaxYearConfig.currentIncomeFallDifferenceAmount
  ).thenReturn(
    2500
  )

  when(
    mockTCTaxYearConfig.wtc
  ).thenReturn(
    mockWTC
  )

  when(
    mockWTC.basicElement
  ).thenReturn(
    1960
  )

  when(
    mockWTC.coupleElement
  ).thenReturn(
    2010
  )

  when(
    mockWTC.loneParentElement
  ).thenReturn(
    2010
  )

  when(
    mockWTC.hours30Element
  ).thenReturn(
    810
  )

  when(
    mockWTC.disabledWorkerElement
  ).thenReturn(
    3000
  )

  when(
    mockWTC.severeDisabilityWorkerElement
  ).thenReturn(
    1290
  )

  when(
    mockWTC.maxChildcareOneChildElement
  ).thenReturn(
    175
  )

  when(
    mockWTC.maxChildcareMoreChildrenElement
  ).thenReturn(
    300
  )

  when(
    mockWTC.eligibleCostCoveredPercent
  ).thenReturn(
    70
  )

  when(
    mockTCTaxYearConfig.ctc
  ).thenReturn(
    mockCTC
  )

  when(
    mockCTC.childElement
  ).thenReturn(
    2780
  )

  when(
    mockCTC.youngPersonElement
  ).thenReturn(
    2780
  )

  when(
    mockCTC.disabledChildElement
  ).thenReturn(
    3175
  )

  when(
    mockCTC.severeDisabilityChildElement
  ).thenReturn(
    1290
  )

  when(
    mockCTC.familyElement
  ).thenReturn(
    545
  )

  when(
    mockTCTaxYearConfig.thresholds
  ).thenReturn(
    mockThresholds
  )

  when(
    mockThresholds.taperRatePercent
  ).thenReturn(
    41
  )

  when(
    mockThresholds.ctcIncomeThreshold
  ).thenReturn(
    16105
  )

  when(
    mockThresholds.wtcIncomeThreshold
  ).thenReturn(
    6420
  )

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    reset(tcCalculator.tcConfig)

    when(
      tcCalculator.tcConfig.monthsInTaxYear
    ).thenReturn(
      12
    )

    when(
      tcCalculator.tcConfig.getConfig(any[LocalDate]())
    ).thenReturn(
      mockTCTaxYearConfig
    )

    when(
      tcCalculator.tcConfig.getCurrentTaxYearDateRange(any[LocalDate])
    ).thenReturn(
      (parseDate("2016-04-06"), parseDate("2017-04-06"))
    )

  }

  "TCCalculatorHelpers" when {
    val SUT = new TCCalculatorHelpers {
      override val tcConfig: TCConfig = mock[TCConfig]
    }
    when(
      SUT.tcConfig.getCurrentTaxYearDateRange(any[LocalDate])
    ).thenReturn(
      (parseDate("2016-09-27"), parseDate("2017-04-06"))
    )

    when(
      SUT.tcConfig.getConfig(any[LocalDate]())
    ).thenReturn(
      mockTCTaxYearConfig
    )

    "calling amountForDateRange" should {

      "return 0" when {
        "fromDate is after toDate" in {
          val fromDate = parseDate("2016-05-01")
          val toDate = parseDate("2016-04-06")
          val result = SUT.amountForDateRange(1000, Periods.Monthly, fromDate, toDate)
          result shouldBe 0
        }

        "fromDate is the same as toDate" in {
          val fromDate = parseDate("2016-04-06")
          val toDate = parseDate("2016-04-06")
          val result = SUT.amountForDateRange(1000, Periods.Monthly, fromDate, toDate)
          result shouldBe 0
        }
      }

      "return rounded daily amount by number of days" when {
        "fromDate is before toDate" in {
          val SUT = new TCCalculatorHelpers {
            override val tcConfig: TCConfig = mock[TCConfig]
            when(
              tcConfig.getCurrentTaxYearDateRange(any[LocalDate])
            ).thenReturn(
              (parseDate("2016-09-27"), parseDate("2017-04-06"))
            )

            override def daysBetween(fromDate: LocalDate, toDate: LocalDate): Int = 10

            override def amountFromPeriodToDaily(cost: BigDecimal, fromPeriod: Periods.Period, daysInTheYear: Int): BigDecimal = 10.49468
          }

          val fromDate = parseDate("2016-04-06")
          val toDate = parseDate("2016-05-01")
          val result = SUT.amountForDateRange(1000, Periods.Monthly, fromDate, toDate)
          result shouldBe 104.90
        }
      }

    }

    "calling getTotalMaximumAmountPerPeriod" should {
      val fromDate = parseDate("2016-09-27")
      val untilDate = parseDate("2017-04-06")

      "return the sum of maxAmounts for wtcWork, wtcChildcare, ctcIndividual and ctcFamily elements" when {
        "elements are determined" in {
          val period = Period(
            from = fromDate,
            until = untilDate,
            elements = Elements(
              wtcWorkElement = Element(
                maximumAmount = 4737.70
              ),
              wtcChildcareElement = Element(
                maximumAmount = 728.80
              ),
              ctcIndividualElement = Element(
                maximumAmount = 3400.20
              ),
              ctcFamilyElement = Element(
                maximumAmount = 1300.50
              )
            )
          )
          val totalMaximumAmount = SUT.getTotalMaximumAmountPerPeriod(period)
          totalMaximumAmount shouldBe 10167.20
        }
      }

      "return 0" when {
        "max amounts for elements are not determined in the period" in {
          val period = Period(
            from = fromDate,
            until = untilDate,
            elements = Elements(
              wtcWorkElement = Element(),
              wtcChildcareElement = Element(),
              ctcIndividualElement = Element(),
              ctcFamilyElement = Element()
            )
          )
          val totalMaximumAmount = SUT.getTotalMaximumAmountPerPeriod(period)
          totalMaximumAmount shouldBe 0.00
        }
      }
    }
  }

  "TCCalculatorTapering" when {
    val SUT = new TCCalculatorTapering {
      override val tcConfig: TCConfig = mock[TCConfig]
    }

    "calling isTaperingRequiredForElements" should {
      val threshold = 6420

      "return true" when {
        "income is more than threshold" in {
          val income = threshold + 0.01
          val result = SUT.isTaperingRequiredForElements(income, threshold)
          result shouldBe true
        }
      }

      "return false" when {
        "income is equal to threshold" in {
          val income = threshold
          val result = SUT.isTaperingRequiredForElements(income, threshold)
          result shouldBe false
        }

        "income is more than threshold" in {
          val income = threshold - 0.01
          val result = SUT.isTaperingRequiredForElements(income, threshold)
          result shouldBe false
        }
      }
    }

    "calling getPercentOfAmount" should {
      "determine correctly percentage of an amount" in {
        val amount = 1000
        val percentage = 20
        val result = SUT.getPercentOfAmount(amount, percentage)
        result shouldBe 200
      }
    }

    "calling taperFirstElement" should {

      val fromDate = parseDate("2016-09-27")
      val untilDate = parseDate("2017-04-06")
      val wtcIncomeThreshold = 6420
      val income = 17000
      val wtcMaxAmount = 5837.70

      val inputPeriod = TCPeriod(
        from = fromDate,
        until = untilDate,
        householdElements = TCHouseholdElements(),
        claimants = List(),
        children = List()
      )

      val setup =  Period(
        from = fromDate,
        until = untilDate,
        elements = Elements(
          wtcWorkElement = Element(
            maximumAmount = wtcMaxAmount
          ),
          wtcChildcareElement = Element(),
          ctcIndividualElement = Element(),
          ctcFamilyElement = Element()
        )
      )

      "do full taparing" when {

        "netAmount is 0 (user earnings are above or equal to WTC maximum amount)" in {
          val SUT = new TCCalculatorTapering {
            override val tcConfig: TCConfig = mock[TCConfig]
            override def earningsAmountToTaperForPeriod(
                                                         income: BigDecimal,
                                                         thresholdIncome: BigDecimal,
                                                         period: TCPeriod
                                                         ): BigDecimal = wtcMaxAmount + 0.01

            override def netAmountPerElementPerPeriod(
                                                       taperAmount: BigDecimal,
                                                       maximumAmountPerElement: BigDecimal
                                                       ): BigDecimal = 0
          }

          val result = SUT.taperFirstElement(
            period = setup,
            inputPeriod = inputPeriod,
            income = income,
            wtcIncomeThreshold = wtcIncomeThreshold
          )
          result shouldBe setup.copy(
            elements = setup.elements.copy(
              wtcWorkElement = setup.elements.wtcWorkElement.copy(
                netAmount = 0,
                taperAmount = wtcMaxAmount
              )
            )
          )
        }

      }

      "do partial taparing" when {

        "netAmount more than 0 (user earnings are bellow WTC maximum amount)" in {
          val SUT = new TCCalculatorTapering {
            override val tcConfig: TCConfig = mock[TCConfig]
            override def earningsAmountToTaperForPeriod(
                                                         income: BigDecimal,
                                                         thresholdIncome: BigDecimal,
                                                         period: TCPeriod
                                                         ): BigDecimal = wtcMaxAmount - 0.01

            override def netAmountPerElementPerPeriod(
                                                       taperAmount: BigDecimal,
                                                       maximumAmountPerElement: BigDecimal
                                                       ): BigDecimal = 0.01
          }

          val result = SUT.taperFirstElement(
            period = setup,
            inputPeriod = inputPeriod,
            income = income,
            wtcIncomeThreshold = wtcIncomeThreshold
          )
          result shouldBe setup.copy(
            elements = setup.elements.copy(
              wtcWorkElement = setup.elements.wtcWorkElement.copy(
                netAmount = 0.01,
                taperAmount = wtcMaxAmount - 0.01
              )
            )
          )
        }

      }


    }

    "calling taperSecondElement" when {

      val fromDate = parseDate("2016-09-27")
      val untilDate = parseDate("2017-04-06")
      val wtcIncomeThreshold = 6420
      val income = 17000
      val wtcMaxAmount = 5837.70
      val wtcChildcareMaxAmount = 728.80

      val inputPeriod = TCPeriod(
        from = fromDate,
        until = untilDate,
        householdElements = TCHouseholdElements(),
        claimants = List(),
        children = List()
      )

      "wtcWorkElement.netAmount is 0" should {
        "assign the max amount for chldcare element without any tapering" in {
          val SUT = new TCCalculatorTapering {
            override val tcConfig: TCConfig = mock[TCConfig]
            override def earningsAmountToTaperForPeriod(
                                                         income: BigDecimal,
                                                         thresholdIncome: BigDecimal,
                                                         period: TCPeriod
                                                         ): BigDecimal = 0

            override def netAmountPerElementPerPeriod(
                                                       taperAmount: BigDecimal,
                                                       maximumAmountPerElement: BigDecimal
                                                       ): BigDecimal = 0
          }

          val setup = Period(
            from = fromDate,
            until = untilDate,
            elements = Elements(
              wtcWorkElement = Element(
                maximumAmount = wtcMaxAmount,
                netAmount = 58.00,
                taperAmount = 5494
              ),
              wtcChildcareElement = Element(
                maximumAmount = wtcChildcareMaxAmount
              ),
              ctcIndividualElement = Element(
                maximumAmount = 5504.20
              ),
              ctcFamilyElement = Element(
                maximumAmount = 547.50
              )
            )
          )

          val result = SUT.taperSecondElement(setup, inputPeriod, income, wtcIncomeThreshold)
          result shouldBe setup.copy(
            elements = setup.elements.copy(
              wtcChildcareElement = setup.elements.wtcChildcareElement.copy(
                netAmount = wtcChildcareMaxAmount,
                taperAmount = 0
              )
            )
          )
        }
      }

      "wtcWorkElement.netAmount isn't 0" should {
        val setup = Period(
          from = fromDate,
          until = untilDate,
          elements = Elements(
            wtcWorkElement = Element(
              maximumAmount = wtcMaxAmount,
              netAmount = 0,
              taperAmount = wtcMaxAmount
            ),
            wtcChildcareElement = Element(
              maximumAmount = wtcChildcareMaxAmount
            ),
            ctcIndividualElement = Element(
              maximumAmount = 5504.20
            ),
            ctcFamilyElement = Element(
              maximumAmount = 547.50
            )
          )
        )

        "do full tapering if childcare net amout is calculated as 0" in {
          val SUT = new TCCalculatorTapering {
            override val tcConfig: TCConfig = mock[TCConfig]
            override def earningsAmountToTaperForPeriod(
                                                         income: BigDecimal,
                                                         thresholdIncome: BigDecimal,
                                                         period: TCPeriod
                                                         ): BigDecimal = 0

            override def netAmountPerElementPerPeriod(
                                                       taperAmount: BigDecimal,
                                                       maximumAmountPerElement: BigDecimal
                                                       ): BigDecimal = 0
          }

          val result = SUT.taperSecondElement(setup, inputPeriod, income, wtcIncomeThreshold)
          result shouldBe setup.copy(
            elements = setup.elements.copy(
              wtcChildcareElement = setup.elements.wtcChildcareElement.copy(
                netAmount = 0,
                taperAmount = wtcChildcareMaxAmount
              )
            )
          )
        }

        "do partial tapering if childcare net amout is more than 0" in {
          val SUT = new TCCalculatorTapering {
            override val tcConfig: TCConfig = mock[TCConfig]
            override def earningsAmountToTaperForPeriod(
                                                         income: BigDecimal,
                                                         thresholdIncome: BigDecimal,
                                                         period: TCPeriod
                                                         ): BigDecimal = wtcMaxAmount + 0.01

            override def netAmountPerElementPerPeriod(
                                                       taperAmount: BigDecimal,
                                                       maximumAmountPerElement: BigDecimal
                                                       ): BigDecimal = 0.01
          }

          val result = SUT.taperSecondElement(setup, inputPeriod, income, wtcIncomeThreshold)
          result shouldBe setup.copy(
            elements = setup.elements.copy(
              wtcChildcareElement = setup.elements.wtcChildcareElement.copy(
                netAmount = 0.01,
                taperAmount = 0.01
              )
            )
          )
        }
      }
    }

    "calling taperThirdElement" should {

      "taper CTC (Child element) corectly" in {

        val fromDate = parseDate("2016-09-27")
        val untilDate = parseDate("2017-04-06")

        val wtcIncomeThreshold = 6420
        val ctcIncomeThreshold = 16105
        val income = 17000

        val inputPeriod = TCPeriod(
          from = fromDate,
          until = untilDate,
          householdElements = TCHouseholdElements(),
          claimants = List(),
          children = List()
        )

        val setup = Period(
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


        val SUT = new TCCalculatorTapering {
          override val tcConfig: TCConfig = mock[TCConfig]
          override def buildTaperingThresholdVal(
                                         condition: Boolean,
                                         inputPeriod: models.input.tc.TCPeriod,
                                         income: BigDecimal,
                                         wtcIncomeThreshold: BigDecimal,
                                         ctcIncomeThreshold: BigDecimal
                                         ): Option[BigDecimal] = None

          override def getNetAmount(taperingThresholdVal: Option[BigDecimal],
                                    maximumAmount: BigDecimal,
                                    income: BigDecimal,
                                    inputPeriod: models.input.tc.TCPeriod): BigDecimal = 100

          override def getTaperAmount(taperingThresholdVal: Option[BigDecimal],
                                      maximumAmount: BigDecimal, income: BigDecimal,
                                      inputPeriod: models.input.tc.TCPeriod): BigDecimal = 200
        }

        val result = SUT.taperThirdElement(setup, inputPeriod, income, wtcIncomeThreshold, ctcIncomeThreshold)
        result._1 shouldBe setup.copy(
          elements = setup.elements.copy(
            ctcIndividualElement = setup.elements.ctcIndividualElement.copy(
              netAmount = 100,
              taperAmount = 200
            )
          )
        )
        result._2 shouldBe false
      }
    }

  }


  "tcCalculator" should {

    val fromDate = parseDate("2016-09-27")
    val untilDate = parseDate("2017-04-06")

    val inputPeriod = TCPeriod(
      from = fromDate,
      until = untilDate,
      householdElements = TCHouseholdElements(),
      claimants = List(),
      children = List()
    )

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

    "determine wtc elements" should {
      val basicPeriod = new models.input.tc.TCPeriod(
        from = fromDate,
        until = untilDate,
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

      "(qualifying) determine if get basic element and the amount for the period" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            basic = true
          )
        )
        val basicElement = tcCalculator.basicElementForPeriod(period)
        basicElement shouldBe BigDecimal(1025.67)
      }

      "(non qualifying) determine if get basic element and the amount for the period" in {
        val getsBasicElement = tcCalculator.basicElementForPeriod(basicPeriod)
        getsBasicElement shouldBe BigDecimal(0.00)
      }

      "determine if get 30 hours element and the amount for the period" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            hours30 = true
          )
        )
        val hours30Element = tcCalculator.hours30ElementForPeriod(period)
        hours30Element shouldBe BigDecimal(424.02)
      }

      "determine if get disabled worker element and the amount for the period" in {
        val period = basicPeriod.copy(
          claimants = List(
            TCClaimant(
              qualifying = true,
              isPartner = false,
              claimantDisability = TCDisability(disability = true, severeDisability = false),
              doesNotTaper = false
            )
          )
        )
        val workerDisabiltyElement = tcCalculator.disabledWorkerElementForPeriod(period, period.claimants.head)
        workerDisabiltyElement shouldBe BigDecimal(1570.02)
      }

      "determine if get severely disabled worker element and the amount for the period" in {
        val period = basicPeriod.copy(
          claimants = List(
            TCClaimant(
              qualifying = true,
              isPartner = false,
              claimantDisability = TCDisability(disability = false, severeDisability = true),
              doesNotTaper = false
            )
          )
        )
        val severelyDisabledWorkerElement = tcCalculator.severelyDisabledWorkerElementForPeriod(period, period.claimants.head)
        severelyDisabledWorkerElement shouldBe BigDecimal(674.23)
      }

      "determine if get lone parent element and amount for the period" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            loneParent = true
          )
        )
        val loneParentElement = tcCalculator.loneParentElementForPeriod(period)
        loneParentElement shouldBe BigDecimal(1052.41)
      }

      "determine if get second adult element and amount for the period" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            secondParent = true
          )
        )
        val coupleElement = tcCalculator.secondAdultElementForPeriod(period)
        coupleElement shouldBe BigDecimal(1052.41)
      }

      "(qualifying) determine if get family element for period and amount for the period" in {
        val period =  basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            family = true
          )
        )
        val familyElement = tcCalculator.maxFamilyElementForPeriod(period)
        familyElement shouldBe BigDecimal(284.59)
      }

      "(non - qualifying) determine if get family element for period and amount for the period" in {
        val familyElement = tcCalculator.maxFamilyElementForPeriod(basicPeriod)
        familyElement shouldBe BigDecimal(0.00)
      }

      "(qualifying) Determine if child gets the child basic element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Weekly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val childElement = tcCalculator.childOrYoungAdultBasicElementForPeriod(period, period.children.head)
        childElement shouldBe BigDecimal(1455.42)
      }

      "(non-qualifying) Determine if child gets the child basic element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val childElement = tcCalculator.childOrYoungAdultBasicElementForPeriod(period, period.children.head)
        childElement shouldBe BigDecimal(0.00)
      }

      "(qualifying) Determine if child gets disability element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = true,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val childDisabiltyElement = tcCalculator.childOrYoungAdultDisabilityElementForPeriod(period, period.children.head)
        childDisabiltyElement shouldBe BigDecimal(1661.70)
      }

      "(non - qualifying) Determine if child gets disability element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = false,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val childDisabiltyElement = tcCalculator.childOrYoungAdultDisabilityElementForPeriod(period, period.children.head)
        childDisabiltyElement shouldBe BigDecimal(0.00)
      }

      "(qualifying) Determine if child gets severe disability element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = true,
                childcare = false
              )
            )
          )
        )
        val childSevereDisabiltyElement = tcCalculator.childOrYoungAdultSevereDisabilityElementForPeriod(period, period.children.head)
        childSevereDisabiltyElement shouldBe BigDecimal(674.23)
      }

      "(non-qualifying) Determine if child gets severe disability element for the period" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val childSevereDisabiltyElement = tcCalculator.childOrYoungAdultSevereDisabilityElementForPeriod(period, period.children.head)
        childSevereDisabiltyElement shouldBe BigDecimal(0.00)
      }


      "(qualifying) determine child element(s) (as a total) for multiple children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 2000,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 3000,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = true,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 2000,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = true,
                severeDisability = true,
                childcare = true
              )
            )
          )
        )
        val childElement = tcCalculator.maxChildElementForPeriod(period)
        childElement shouldBe BigDecimal(8363.89)
      }

      "(one child qualifying, one not qualifying)(child + child) determine child element(s) (as a total) for multiple children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = true,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = false,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = false,
                disability = true,
                severeDisability = true,
                childcare = true
              )
            )
          )
        )
        val period1ChildElement = tcCalculator.maxChildElementForPeriod(period)
        period1ChildElement shouldBe BigDecimal(3117.12)
      }

      "(both not qualifying)(child + child) determine child element(s) (as a total) for multiple children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = false,
                disability = true,
                severeDisability = false,
                childcare = false
              )
            ),
            TCChild(
              qualifying = false,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = false,
                disability = true,
                severeDisability = true,
                childcare = false
              )
            )
          )
        )
        val childElement = tcCalculator.maxChildElementForPeriod(period)
        childElement shouldBe BigDecimal(0.00)
      }

      "(qualifying)(young adult + child) determine child element(s) (as a total) for multiple children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )
        val period1ChildElement = tcCalculator.maxChildElementForPeriod(period)
        period1ChildElement shouldBe BigDecimal(2910.84)
      }


      "(no children) return BigDecimal(0.00) when checking weekly threshold spend for children" in {
        val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
        val result = tcCalculator invokePrivate decoratedChildCareThreshold(basicPeriod)
        result shouldBe BigDecimal(0.00)
      }

      "(1 child) return 1 child threshold when checking weekly threshold spend for children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )

        val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
        val result = tcCalculator invokePrivate decoratedChildCareThreshold(period)
        result shouldBe BigDecimal(175.00)
      }

      "(2 children) return 1 child threshold when 1 child has no childcare cost when checking weekly threshold spend for children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 0,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )

        val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
        val result = tcCalculator invokePrivate decoratedChildCareThreshold(period)
        result shouldBe BigDecimal(175.00)
      }

      "(3 children) return multiple child threshold when checking weekly threshold spend for children" in {
        val period = basicPeriod.copy(
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = false,
                youngAdult = true,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 200,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )

        val decoratedChildCareThreshold = PrivateMethod[BigDecimal]('getChildcareThresholdPerWeek)
        val result = tcCalculator invokePrivate decoratedChildCareThreshold(period)
        result shouldBe BigDecimal(300.00)
      }

      "(claimant with partner both qualifying) determine wtc work element(s) (as a total) for multiple claimants" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            basic = true,
            childcare = true,
            secondParent = true,
            family = true
          ),
          claimants = List(
            TCClaimant(
              qualifying = true,
              isPartner = false,
              claimantDisability = TCDisability(
                disability = true,
                severeDisability = false
              ),
              doesNotTaper = false
            ),
            TCClaimant(
              qualifying = true,
              isPartner = true,
              claimantDisability = TCDisability(
                disability = true,
                severeDisability = false
              ),
              doesNotTaper = false
            )
          )
        )
        val workElement = tcCalculator.maxWorkElementForPeriod(period)
        workElement shouldBe BigDecimal(5218.12)
      }

      "Determine WTC childcare element when there is only one child (not exceeding the element limit)" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            basic = true,
            childcare = true,
            loneParent = true,
            family = true
          ),
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 46.15,
              childcareCostPeriod = Periods.Weekly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )
        val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period)
        wtcChildcareElement shouldBe BigDecimal(878.41)
      }

      "Determine WTC childcare element when there is only one child and for second one childcare element is false (exceeding the element limit)" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            basic = true,
            childcare = true,
            loneParent = true,
            family = true
          ),
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 800,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = true,
              childcareCost = 800,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = false
              )
            )
          )
        )
        val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period)
        wtcChildcareElement shouldBe BigDecimal(3333.14)
      }

      "Determine WTC childcare element when there is only one child and second is not qualifying (exceeding the element limit)" in {
        val period = basicPeriod.copy(
          householdElements = basicPeriod.householdElements.copy(
            basic = true,
            childcare = true,
            loneParent = true,
            family = true
          ),
          children = List(
            TCChild(
              qualifying = true,
              childcareCost = 800,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            ),
            TCChild(
              qualifying = false,
              childcareCost = 800,
              childcareCostPeriod = Periods.Monthly,
              childElements = TCChildElements(
                child = true,
                youngAdult = false,
                disability = false,
                severeDisability = false,
                childcare = true
              )
            )
          )
        )
        val wtcChildcareElement = tcCalculator.maxChildcareElementForPeriod(period)
        wtcChildcareElement shouldBe BigDecimal(3333.14)
      }

    }

    "Determine award period start and end dates" when {
      val fromDate = parseDate("2016-09-27")
      val toDate = parseDate("2017-04-06")
      "there is only one period in each of two taxyears" in {
        val input = TCCalculatorInput(
          taxYears = List(
            TCTaxYear(
              from = parseDate("2016-09-27"),
              until = parseDate("2017-04-06"),
              previousHouseholdIncome = TCIncome(None, None, None, None, None),
              currentHouseholdIncome = TCIncome(None, None, None, None, None),
              periods = List(
                TCPeriod(
                  from = fromDate,
                  until = toDate,
                  householdElements = TCHouseholdElements(),
                  claimants = List.empty,
                  children = List.empty
                )
              )
            ),
            TCTaxYear(
              from = parseDate("2017-09-27"),
              until = parseDate("2018-04-06"),
              previousHouseholdIncome = TCIncome(None, None, None, None, None),
              currentHouseholdIncome = TCIncome(None, None, None, None, None),
              periods = List(
                TCPeriod(
                  from = fromDate,
                  until = toDate,
                  householdElements = TCHouseholdElements(),
                  claimants = List.empty,
                  children = List.empty
                )
              )
            )
          )
        )
        val award = await(tcCalculator.award(input))
        award.from shouldBe parseDate("2016-09-27")
        award.until shouldBe parseDate("2018-04-06")
      }
      "there is only one period" in {
        val input = TCCalculatorInput(
          taxYears = List(
            TCTaxYear(
              from = fromDate,
              until = toDate,
              previousHouseholdIncome = TCIncome(None, None, None, None, None),
              currentHouseholdIncome = TCIncome(None, None, None, None, None),
              periods = List(
                TCPeriod(
                  from = fromDate,
                  until = toDate,
                  householdElements = TCHouseholdElements(),
                  claimants = List.empty,
                  children = List.empty
                )
              )
            )
          )
        )
        val award = await(tcCalculator.award(input))
        award.from shouldBe fromDate
        award.until shouldBe toDate
      }

      "there is more than one period" in {
        val input = TCCalculatorInput(
          taxYears = List(
            TCTaxYear(
              from = fromDate,
              until = toDate,
              previousHouseholdIncome = TCIncome(None, None, None, None, None),
              currentHouseholdIncome = TCIncome(None, None, None, None, None),
              periods = List(
                TCPeriod(
                  from = fromDate,
                  until = toDate,
                  householdElements = TCHouseholdElements(),
                  claimants = List.empty,
                  children = List.empty
                ),
                TCPeriod(
                  from = toDate,
                  until = fromDate.plusYears(1),
                  householdElements = TCHouseholdElements(),
                  claimants = List.empty,
                  children = List.empty
                )
              )
            )
          )
        )
        val award = await(tcCalculator.award(input))
        award.from shouldBe fromDate
        award.until shouldBe toDate
      }
    }

    "Determine which amount is higher (one amount is higher)" in {
      val amount = BigDecimal(100.00)
      val higherAmount = BigDecimal(100.0001)
      val result = tcCalculator.getHigherAmount(amount, higherAmount)
      result shouldBe higherAmount
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

      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = false, claimantDisability = TCDisability())

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

      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = false, isPartner = false, claimantDisability = TCDisability())

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

      val claimant = models.input.tc.TCClaimant(qualifying = true, isPartner = false, claimantDisability = TCDisability())
      val claimantClaimingSocialSecurityBenefit = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = true, claimantDisability = TCDisability())

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

      val claimantClaimingSocialSecurityBenefit1 = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = false, claimantDisability = TCDisability())
      val claimantClaimingSocialSecurityBenefit2 = models.input.tc.TCClaimant(qualifying = true, doesNotTaper = true, isPartner = true, claimantDisability = TCDisability())

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

      val claimant1 = models.input.tc.TCClaimant(qualifying = true, isPartner = false, claimantDisability = TCDisability())
      val claimant2 = models.input.tc.TCClaimant(qualifying = true, isPartner = true, claimantDisability = TCDisability())

      val inputPeriod = models.input.tc.TCPeriod(from = fromDate, until = untilDate, householdElements = TCHouseholdElements(), claimants = List(claimant1, claimant2), children = List())

      val result = tcCalculator.generateRequiredAmountsPerPeriod(period, inputPeriod, income, wtcThreshold, ctcThreshold)
      result.elements.wtcWorkElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.wtcChildcareElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcIndividualElement.netAmount shouldBe BigDecimal(0.00)
      result.elements.ctcFamilyElement.netAmount shouldBe BigDecimal(0.00)
    }

    "generate the maximum amounts for a period model" in {
      val period = models.input.tc.TCPeriod(
        from = fromDate,
        until = untilDate,
        householdElements = TCHouseholdElements(
          basic = true,
          hours30 = false,
          childcare = true,
          loneParent = true,
          secondParent = false,
          family = true
        ),
        claimants = List(
          TCClaimant(
            qualifying = true,
            isPartner = false,
            claimantDisability = TCDisability(
              disability = false,
              severeDisability = false
            ),
            doesNotTaper = false
          )
        ),
        children = List(
          TCChild(
            qualifying = true,
            childcareCost = 200,
            childcareCostPeriod = Periods.Monthly,
            childElements = TCChildElements(
              child = true,
              youngAdult = false,
              disability = false,
              severeDisability = false,
              childcare = true
            )
          )
        )
      )
      val setup = tcCalculator.generateMaximumAmountsForPeriod(period)
      setup.elements.wtcWorkElement.maximumAmount shouldBe BigDecimal(2078.08)
      setup.elements.wtcChildcareElement.maximumAmount shouldBe BigDecimal(878.41)
      setup.elements.ctcIndividualElement.maximumAmount shouldBe BigDecimal(1455.42)
      setup.elements.ctcFamilyElement.maximumAmount shouldBe BigDecimal(284.59)
    }

    "Generate the maximum amounts for a period model (no children, get just basic element)" in {
      val period = models.input.tc.TCPeriod(
        from = fromDate,
        until = untilDate,
        householdElements = TCHouseholdElements(
          basic = true,
          hours30 = false,
          childcare = false,
          loneParent = false,
          secondParent = false,
          family = false
        ),
        claimants = List(
          TCClaimant(
            qualifying = true,
            isPartner = false,
            claimantDisability = TCDisability(
              disability = false,
              severeDisability = false
            ),
            doesNotTaper = false
          )
        ),
        children = List()
      )
      val setup = tcCalculator.generateMaximumAmountsForPeriod(period)
      setup.elements.wtcWorkElement.maximumAmount shouldBe BigDecimal(1025.67)
      setup.elements.wtcChildcareElement.maximumAmount shouldBe BigDecimal(0.00)
      setup.elements.ctcIndividualElement.maximumAmount shouldBe BigDecimal(0.00)
      setup.elements.ctcFamilyElement.maximumAmount shouldBe BigDecimal(0.00)
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

    "calculateHouseholdIncome" should {
      "return previous income" when {
        "previous and current incomes are equal" in {
          val income = TCIncome(
            employment = Some(List(2500, 50000)),
            pension = Some(List(100)),
            other = Some(List(50, 25)),
            benefits = Some(List(200, 300)),
            statutory = Some(List(
              TCStatutoryIncome(weeks = 5, amount = 20)
            ))
          )

          val decoratedAdvicePeriod = PrivateMethod[BigDecimal]('calculateHouseholdIncome)
          val result = tcCalculator invokePrivate decoratedAdvicePeriod(LocalDate.now, income, income)
          result shouldBe 57200
        }
      }
    }

    "return adjusted previous income" when {

      "previous income is higher than current one" when {
        "there is no other income" in {
          val previousIncome = TCIncome(
            employment = Some(List(50000)),
            pension = None,
            other = None,
            benefits = None,
            statutory = None
          )

          val currentIncome = TCIncome(
            employment = Some(List(25000)),
            pension = None,
            other = None,
            benefits = None,
            statutory = None
          )

          val decoratedAdvicePeriod = PrivateMethod[BigDecimal]('calculateHouseholdIncome)
          val result = tcCalculator invokePrivate decoratedAdvicePeriod(LocalDate.now, previousIncome, currentIncome)
          result shouldBe 27500
        }

        "other income is less than it's adjustment limit (£300)" in {
          val previousIncome = TCIncome(
            employment = Some(List(50000)),
            pension = None,
            other = Some(List(200)),
            benefits = None,
            statutory = None
          )

          val currentIncome = TCIncome(
            employment = Some(List(25000)),
            pension = None,
            other = None,
            benefits = None,
            statutory = None
          )

          val decoratedAdvicePeriod = PrivateMethod[BigDecimal]('calculateHouseholdIncome)
          val result = tcCalculator invokePrivate decoratedAdvicePeriod(LocalDate.now, previousIncome, currentIncome)
          result shouldBe 27500
        }

        "other income is higher  than it's adjustment limit (£300)" in {
          val previousIncome = TCIncome(
            employment = Some(List(50000)),
            pension = None,
            other = Some(List(500)),
            benefits = None,
            statutory = None
          )

          val currentIncome = TCIncome(
            employment = Some(List(25000)),
            pension = None,
            other = None,
            benefits = None,
            statutory = None
          )

          val decoratedAdvicePeriod = PrivateMethod[BigDecimal]('calculateHouseholdIncome)
          val result = tcCalculator invokePrivate decoratedAdvicePeriod(LocalDate.now, previousIncome, currentIncome)
          result shouldBe 27500
        }
      }

      "previous income is less than current one" in {
        val previousIncome = TCIncome(
          employment = Some(List(25000)),
          pension = None,
          other = None,
          benefits = None,
          statutory = None
        )

        val currentIncome = TCIncome(
          employment = Some(List(50000)),
          pension = None,
          other = None,
          benefits = None,
          statutory = None
        )

        val decoratedAdvicePeriod = PrivateMethod[BigDecimal]('calculateHouseholdIncome)
        val result = tcCalculator invokePrivate decoratedAdvicePeriod(LocalDate.now, previousIncome, currentIncome)
        result shouldBe 47500
      }
    }
  }
}
