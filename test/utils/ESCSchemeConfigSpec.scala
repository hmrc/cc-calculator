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

package utils

import com.typesafe.config.ConfigFactory
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import play.api.Configuration
import utils.ESCConfig._

class ESCSchemeConfigSpec extends FakeCCCalculatorApplication {
  val location = "england"

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


    "Return ESCTaxYearConfig when niCategoryCode is empty" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2016", formatter)
      val config = ESCConfig.getConfig(fromDate,"", location)

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
      val taxYear =  ESCTaxYearConfig(
        post2011MaxExemptionMonthlyBasic = 243.00,
        post2011MaxExemptionMonthlyHigher = 124.00,
        post2011MaxExemptionMonthlyAdditional = 110.00,
        defaultTaxCode = "1100L",
        personalAllowanceRate = 0.00,
        defaultPersonalAllowance = 11000,
        taxBasicRate = 20.00,
        taxBasicBandCapacity = 32000.00,
        taxHigherRate = 40.00,
        taxHigherBandUpperLimit = 150000.00,
        taxAdditionalRate = 45.00,
        taxAdditionalBandLowerLimit= 150000.01,
        niCategory = niCat
      )
      config shouldBe taxYear     
    }

    "Return error for invalid niCategoryCode" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("23-05-2016", formatter)
      try {
        val result = ESCConfig.getConfig(fromDate,"Z", location)
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
    "Default ESC SchemeConfig - NI cat A" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("23-05-2015", formatter)
      val config = ESCConfig.getConfig(now,"A", location)
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
      config shouldBe taxYear
    }
    "(2016) ESC SchemeConfig - NI cat A" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("23-05-2016", formatter)
      val config = ESCConfig.getConfig(now,"A", location)
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
      config shouldBe taxYear
    }

    "(2016) ESC SchemeConfig - NI cat B" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("23-05-2016", formatter)
      val config = ESCConfig.getConfig(now,"B", location)
      val niCat = NiCategory(
        niCategoryCode = "B",
        lelMonthlyLowerLimitForCat = 0.00,
        lelMonthlyUpperLimitForCat = 485.00,
        lelRateForCat = 0.00,
        lelPtMonthlyLowerLimitForCat = 486.00,
        lelPtMonthlyUpperLimitForCat = 672.00,
        lelPtRateForCat = 0.00,
        ptUelMonthlyLowerLimitForCat = 673.00,
        ptUelMonthlyUpperLimitForCat = 3583.00,
        ptUelRateForCat = 5.85,
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
      config shouldBe taxYear
    }

    "(2017) ESC SchemeConfig - NI cat C" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("23-07-2017", formatter)
      val config = ESCConfig.getConfig(now,"C", location)
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
      config shouldBe taxYear
    }

    "(2017) ESC SchemeConfig - NI cat A" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("23-07-2017", formatter)
      val config = ESCConfig.getConfig(now,"A", location)
      val niCat = NiCategory(
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
        aboveUelRateForCat = 2.00
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
      config shouldBe taxYear
    }

    "(2016) ESC SchemeConfig - Default NI cat " in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("05-04-2017", formatter)
      val config = ESCConfig.getConfig(now, "", location)
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
      config shouldBe taxYear
    }

    "ESC SchemeConfig - 2018 Tax Year" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val now = LocalDate.parse("06-04-2018", formatter)
      val config = ESCConfig.getConfig(now,"B", location)
      val niCat = NiCategory(
        niCategoryCode = "B",
        lelMonthlyLowerLimitForCat = 0.00,
        lelMonthlyUpperLimitForCat = 490.00,
        lelRateForCat = 0.00,
        lelPtMonthlyLowerLimitForCat = 491.00,
        lelPtMonthlyUpperLimitForCat = 680.00,
        lelPtRateForCat = 0.00,
        ptUelMonthlyLowerLimitForCat = 681.00,
        ptUelMonthlyUpperLimitForCat = 3753.00,
        ptUelRateForCat = 5.85,
        aboveUelMonthlyLowerLimitForCat = 3754.00,
        aboveUelRateForCat = 2.00
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
      config shouldBe taxYear
    }

    "accessing 2017-2018 ptUelMonthlyUpperLimitForCat" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-09-2017", formatter)
      val escTaxYearConfig = ESCConfig.getConfig(fromDate,"A", location)
      escTaxYearConfig.niCategory.ptUelMonthlyUpperLimitForCat shouldBe 3753.00
    }

    "accessing taxBasicBandCapacity" in {
      val pattern = "dd-MM-yyyy"
      val formatter = DateTimeFormat.forPattern(pattern)
      val fromDate = LocalDate.parse("06-12-2016", formatter)
      val escTaxYearConfig = ESCConfig.getConfig(fromDate,"A", location)
      escTaxYearConfig.taxBasicBandCapacity shouldBe 32000.00
    }

  }
}
