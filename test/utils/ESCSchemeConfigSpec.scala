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

package utils

import com.typesafe.config.ConfigFactory
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.Configuration
import utils.ESCConfig._

class ESCSchemeConfigSpec extends FakeCCCalculatorApplication with Helpers {

  object NICatADefault extends TestHelper {
    val description = "Default ESC SchemeConfig - NI cat A"
    val date = "23-05-2015"
    val location = locationEngland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8164
    val ptUelRateForCat = 12.00
    val NICategory = buildNiCategory(niCategoryCode, ptUelRateForCat)
    val basicNiThresholdUkValue = 0.0

  }

  object NICatAScotlandDefault extends TestHelper {
    val description = "Default ESC SchemeConfig - NI cat A, location scotland"
    val date = "23-05-2015"
    val location = locationScotland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8164
    val ptUelRateForCat = 12.00
    val NICategory = buildNiCategory(niCategoryCode, ptUelRateForCat)
    val basicNiThresholdUkValue = 0.0
  }

  object NICatA2016 extends TestHelper {
    val description = "(2016) ESC SchemeConfig - NI cat A"
    val date = "23-05-2016"
    val location = locationEngland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8112
    val ptUelRateForCat = 12.00
    val NICategory = buildNiCategory(niCategoryCode, ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2016Scotland extends TestHelper {
    val description = "(2016) ESC SchemeConfig - NI cat A, location scotland"
    val date = "23-05-2016"
    val location = locationScotland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8112
    val ptUelRateForCat = 12.00
    val NICategory = buildNiCategory(niCategoryCode, ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatB2016 extends TestHelper {
    val description = "(2016) ESC SchemeConfig - NI cat B"
    val date = "23-05-2016"
    val location = locationEngland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8112
    val ptUelRateForCat = 5.85
    val NICategory = buildNiCategory(niCategoryCode, ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatC2017 extends TestHelper {
    val description = "(2017) ESC SchemeConfig - NI cat C"
    val date = "23-07-2017"
    val location = locationEngland
    val defaultTaxCode = "1150L"
    val defaultPersonalAllowance = 11500
    val niCategoryCode = "C"
    val taxBasicBandCapacity = 33500.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryC(ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatC2017Scotland extends TestHelper {
    val description = "(2017) ESC SchemeConfig - NI cat C, location scotland"
    val date = "23-07-2017"
    val location = locationScotland
    val defaultTaxCode = "1150L"
    val defaultPersonalAllowance = 11500
    val niCategoryCode = "C"
    val taxBasicBandCapacity = 31500.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryC(ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2017 extends TestHelper {
    val description = "(2017) ESC SchemeConfig - NI cat A"
    val date = "23-07-2017"
    val location = locationEngland
    val defaultTaxCode = "1150L"
    val defaultPersonalAllowance = 11500
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 33500.00
    val niLimit = 8164
    val ptUelRateForCat = 2.00
    val NICategory = buildNiCategoryA(2.00)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2016Default extends TestHelper {
    val description = "(2016) ESC SchemeConfig - Default NI cat"
    val date = "05-04-2017"
    val location = locationEngland
    val defaultTaxCode = "1100L"
    val defaultPersonalAllowance = 11000
    val niCategoryCode = "A"
    val taxBasicBandCapacity = 32000.00
    val niLimit = 8112
    val ptUelRateForCat = 12.00
    val NICategory = buildNiCategory("A", ptUelRateForCat)
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2018 extends TestHelper {
    val description = "ESC SchemeConfig - 2018 Tax Year"
    val date = "06-04-2018"
    val location = locationEngland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 34500.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2019 extends TestHelper {
    val description = "ESC SchemeConfig - 2019 Tax Year"
    val date = "06-04-2019"
    val location = locationEngland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 34500.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2020 extends TestHelper {
    val description = "ESC SchemeConfig - 2020 Tax Year"
    val date = "06-04-2020"
    val location = locationEngland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 34500.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 0.00
  }

  object NICatA2018Scotland extends TestHelper {
    val description = "ESC SchemeConfig - 2018 Tax Year, location scotland"
    val date = "06-04-2018"
    val location = locationScotland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 10150.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 46350.00
  }


  object NICatA2019Scotland extends TestHelper {
    val description = "ESC SchemeConfig - 2019 Tax Year, location scotland"
    val date = "06-04-2019"
    val location = locationScotland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 10150.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 46350.00
  }

  object NICatA2020Scotland extends TestHelper {
    val description = "ESC SchemeConfig - 2020 Tax Year, location scotland"
    val date = "06-04-2020"
    val location = locationScotland
    val defaultTaxCode = "1185L"
    val defaultPersonalAllowance = 11850
    val niCategoryCode = "B"
    val taxBasicBandCapacity = 10150.00
    val niLimit = 8164
    val ptUelRateForCat = 0.00
    val NICategory = buildNiCategoryB
    val basicNiThresholdUkValue = 46350.00
  }

  private val bandAndYearTests: List[TestHelper] = {
    List(
      NICatADefault,
      NICatAScotlandDefault,
      NICatA2016,
      NICatA2016Scotland,
      NICatB2016,
      NICatC2017,
      NICatC2017Scotland,
      NICatA2017,
      NICatA2016Default,
      NICatA2018,
      NICatA2018Scotland,
      NICatA2019,
      NICatA2019Scotland,
      NICatA2020,
      NICatA2020Scotland)
  }

  "ESC SchemeConfig" should {

    "(ESC) populate upper months limit from config file" in new ESCConfig {
      upperMonthsLimitValidation shouldBe 100
    }

    "(ESC) populate lower months limit from config file" in new ESCConfig {
      lowerMonthsLimitValidation shouldBe 0
    }

    "(ESC) populate lower claimants limit from config file" in new ESCConfig {
      lowerClaimantsLimitValidation shouldBe 1
    }

    "(ESC) populate lower periods limit from config file" in new ESCConfig {
      lowerPeriodsLimitValidation shouldBe 1
    }

    "(ESC) populate lower tax years limit from config file" in new ESCConfig {
      lowerTaxYearsLimitValidation shouldBe 1
    }

    "(ESC) populate maximum monthly exemption pre-2011 from config file" in new ESCConfig {
      pre2011MaxExemptionMonthly shouldBe 243
    }

    "Return 243 for post-2011-maximum-exemption" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2016", formatter)
      ESCConfig.getMaxBottomBandAllowance(fromDate) shouldBe 243

    }

    "Return 8112 for NI limit for 2016/2017" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2016", formatter)
      ESCConfig.getNILimit(fromDate) shouldBe 8112

    }

    "Return error for invalid niCategoryCode" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2016", formatter)
      try {
        val result = ESCConfig.getConfig(fromDate, "Z", locationEngland)
        result shouldBe a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

    "Return error if the configuration details are not present in the scheme config file for a valid niCategoryCode" in {
      val configuration = Configuration(ConfigFactory.load((new java.io.File("/testconfig-esc.conf").getName)))
      val configs: Seq[play.api.Configuration] = configuration.getConfigSeq("test-esc.rule-change").get
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2017", formatter)
      // fetch the config if it matches the particular year
      val configForTaxYear = getConfigForTaxYear(fromDate, configs)
      try {
        val result = configForTaxYear match {
          case Some(x) =>
            getNiCategory("B", x)
          case _ => 0
        }
        result shouldBe a[NoSuchElementException]
      } catch {
        case e: Exception =>
          e shouldBe a[NoSuchElementException]
      }
    }

    bandAndYearTests.foreach(
      x =>
        s"${x.description}" in {
          val pattern = "dd-MM-yyyy"
          val formatter = DateTimeFormat.forPattern(pattern)
          val now = LocalDate.parse(x.date, formatter)
          val config = ESCConfig.getConfig(now, x.niCategoryCode, x.location)

          val taxYear = ESCTaxYearConfig(
            post2011MaxExemptionMonthlyBasic = 243.00,
            post2011MaxExemptionMonthlyHigher = 124.00,
            post2011MaxExemptionMonthlyAdditional = 110.00,
            defaultTaxCode = x.defaultTaxCode,
            personalAllowanceRate = 0.00,
            defaultPersonalAllowance = x.defaultPersonalAllowance,
            taxStarterRate = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 19.00 else 0,
            taxStarterBandCapacity = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 2000.00 else 0,
            taxBasicRate = 20.00,
            taxBasicBandCapacity = x.taxBasicBandCapacity,
            taxIntermediateRate = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 21.00 else 0,
            taxIntermediateBandCapacity = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 19430.00 else 0,
            taxHigherRateBandCapacity = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 106570.00 else 0,
            taxHigherRate = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 41.00 else 40.00,
            taxHigherBandUpperLimit = 150000.00,
            taxAdditionalRate = if(x.location.equals(locationScotland) && now.isAfter(new LocalDate(2018,4,5))) 46.00 else 45.00,
            taxAdditionalBandLowerLimit = 150000.01,
            niLimit = x.niLimit,
            niCategory = x.NICategory,
            basicNiThresholdUk = x.basicNiThresholdUkValue
          )

          config shouldBe taxYear
        }
    )

    "accessing 2017-2018 ptUelMonthlyUpperLimitForCat" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-09-2017", formatter)
      val escTaxYearConfig = ESCConfig.getConfig(fromDate, "A", locationEngland)
      escTaxYearConfig.niCategory.ptUelMonthlyUpperLimitForCat shouldBe 3753.00
    }

    "accessing taxBasicBandCapacity" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-12-2016", formatter)
      val escTaxYearConfig = ESCConfig.getConfig(fromDate, "A", locationEngland)
      escTaxYearConfig.taxBasicBandCapacity shouldBe 32000.00
    }

  }

}

sealed trait Helpers {
  val locationEngland = "england"
  val locationScotland = "scotland"

  def buildNiCategory(code: String, ptUelRateForCat: Double, aboveUelRateForCat: Double = 2.00): NiCategory = {
    NiCategory(
      niCategoryCode = code,
      lelMonthlyLowerLimitForCat = 0.00,
      lelMonthlyUpperLimitForCat = 485.00,
      lelRateForCat = 0.00,
      lelPtMonthlyLowerLimitForCat = 486.00,
      lelPtMonthlyUpperLimitForCat = 672.00,
      lelPtRateForCat = 0.00,
      ptUelMonthlyLowerLimitForCat = 673.00,
      ptUelMonthlyUpperLimitForCat = 3583.00,
      ptUelRateForCat = ptUelRateForCat,
      aboveUelMonthlyLowerLimitForCat = 3584.00,
      aboveUelRateForCat = aboveUelRateForCat
    )
  }

  def buildNiCategoryC(ptUelRateForCat: Double): NiCategory = {
    NiCategory(
      niCategoryCode = "C",
      lelMonthlyLowerLimitForCat = 0.00,
      lelMonthlyUpperLimitForCat = 490.00,
      lelRateForCat = 0.00,
      lelPtMonthlyLowerLimitForCat = 491.00,
      lelPtMonthlyUpperLimitForCat = 680.00,
      lelPtRateForCat = 0.00,
      ptUelMonthlyLowerLimitForCat = 681.00,
      ptUelMonthlyUpperLimitForCat = 3753.00,
      ptUelRateForCat = ptUelRateForCat,
      aboveUelMonthlyLowerLimitForCat = 3754.00,
      aboveUelRateForCat = 0.00
    )
  }

  def buildNiCategoryA(aboveUelRateForCat: Double): NiCategory = {
    NiCategory(
      niCategoryCode = "A",
      lelMonthlyLowerLimitForCat = 0.00,
      lelMonthlyUpperLimitForCat = 490.00,
      lelRateForCat = 0.00,
      lelPtMonthlyLowerLimitForCat = 491.00,
      lelPtMonthlyUpperLimitForCat = 680.00,
      lelPtRateForCat = 0.00,
      ptUelMonthlyLowerLimitForCat = 681.00,
      ptUelMonthlyUpperLimitForCat = 3753.00,
      ptUelRateForCat = 12.00,
      aboveUelMonthlyLowerLimitForCat = 3754.00,
      aboveUelRateForCat = aboveUelRateForCat
    )
  }

  def buildNiCategoryB: NiCategory = {
    NiCategory(
      niCategoryCode = "B",
      lelMonthlyLowerLimitForCat = 0.00,
      lelMonthlyUpperLimitForCat = 503.00,
      lelRateForCat = 0.00,
      lelPtMonthlyLowerLimitForCat = 504.00,
      lelPtMonthlyUpperLimitForCat = 698.00,
      lelPtRateForCat = 0.00,
      ptUelMonthlyLowerLimitForCat = 699.00,
      ptUelMonthlyUpperLimitForCat = 3861.00,
      ptUelRateForCat = 5.85,
      aboveUelMonthlyLowerLimitForCat = 3862.00,
      aboveUelRateForCat = 2.00
    )
  }

  trait TestHelper {
    val description: String
    val date: String
    val location: String
    val defaultTaxCode: String
    val defaultPersonalAllowance: Int
    val niCategoryCode: String
    val taxBasicBandCapacity: Double
    val niLimit: Int
    val ptUelRateForCat: Double
    val NICategory: NiCategory
    val basicNiThresholdUkValue: Double
  }
}
