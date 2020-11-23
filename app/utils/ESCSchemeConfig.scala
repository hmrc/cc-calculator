/*
 * Copyright 2020 HM Revenue & Customs
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

import config.AppConfig
import javax.inject.Inject
import org.joda.time.LocalDate
import play.api.Configuration
import play.api.i18n.Lang

class ESCConfig @Inject()(appConfig: AppConfig,
                          configuration: Configuration) extends CCConfig(appConfig) with MessagesObject {

  private implicit val lang: Lang = Lang("en")

  lazy val pre2011MaxExemptionMonthly: Double = configuration
    .get[Double](s"esc.pre-2011-maximum-exemption.basic-higher-additional.monthly")

  def getConfig(currentDate: LocalDate, niCategoryCode: String, location: String): ESCTaxYearConfig = {
    val configs: Seq[Configuration] = configuration.get[Seq[Configuration]]("esc.rule-change")

    // get the default config and keep
    val defaultConfig =
      configs.find(_.get[String]("rule-date").contains("default")).head

    // fetch the config if it matches the particular year
    val result = getConfigForTaxYear(currentDate, configs).getOrElse(defaultConfig)
    getTaxYear(niCategoryCode, result, location)
  }

  def getLatestConfig(currentDate: LocalDate): Configuration = {
    val configs: Seq[Configuration] =
      configuration.get[Seq[Configuration]]("esc.rule-change")

    // get the default config and keep
    val defaultConfig =
      configs.find(_.get[String]("rule-date").contains("default")).head

    // fetch the config if it matches the particular year
    getConfigForTaxYear(currentDate, configs).getOrElse(defaultConfig)
  }

  def getNILimit(currentDate: LocalDate): Double = {
    val result = getLatestConfig(currentDate)
    result.get[Double]("ni-limit")
  }

  def getMaxBottomBandAllowance(currentDate: LocalDate): Double = {
    val result = getLatestConfig(currentDate)
    result.get[Double]("post-2011-maximum-exemption.basic.monthly")
  }

  private def getTaxYear(niCategoryCode: String, config: Configuration, location: String): ESCTaxYearConfig = {
    // get the ni Category
    val niCat = getNiCategory(niCategoryCode, config)

    val localConfig = config.getConfig(s"tax.${location.toLowerCase}").getOrElse(config.getConfig("tax.default").get)

    ESCTaxYearConfig(
      post2011MaxExemptionMonthlyBasic = config.getDouble("post-2011-maximum-exemption.basic.monthly").get,
      post2011MaxExemptionMonthlyHigher = config.getDouble("post-2011-maximum-exemption.higher.monthly").get,
      post2011MaxExemptionMonthlyAdditional = config.getDouble("post-2011-maximum-exemption.additional.monthly").get,
      defaultTaxCode = localConfig.getString("default-tax-code").get,
      personalAllowanceRate = localConfig.getDouble("personal-allowance.rate").get,
      defaultPersonalAllowance = localConfig.getDouble("personal-allowance.default-personal-allowance").get,
      taxStarterRate = localConfig.getDouble("starter.rate").getOrElse(0),
      taxStarterBandCapacity = localConfig.getDouble("starter.band-capacity-annual-amount").getOrElse(0),
      taxBasicRate = localConfig.getDouble("basic.rate").get,
      taxBasicBandCapacity = localConfig.getDouble("basic.band-capacity-annual-amount").get,
      taxIntermediateRate = localConfig.getDouble("intermediate.rate").getOrElse(0),
      taxIntermediateBandCapacity = localConfig.getDouble("intermediate.band-capacity-annual-amount").getOrElse(0),
      taxHigherRateBandCapacity = localConfig.getDouble("higher.band-capacity-annual-amount").getOrElse(0),
      taxHigherRate = localConfig.getDouble("higher.rate").get,
      taxHigherBandUpperLimit = localConfig.getDouble("higher.band-annual-upper-limit").getOrElse(0),
      taxAdditionalRate = localConfig.getDouble("additional.rate").get,
      taxAdditionalBandLowerLimit = localConfig.getDouble("additional.band-annual-lower-limit").get,
      niLimit = config.getDouble("ni-limit").get,
      niCategory = niCat,
      basicNiThresholdUk = localConfig.getDouble("basic-ni-threshold-uk").getOrElse(0)
    )
  }

  def getNiCategory(niCategoryCode: String, config: Configuration): NiCategory ={
    // get the ni Category
    val niCode = niCategoryCode match {
      case cat if cat.isEmpty => config.get[String]("default-ni-code")
      case cat if cat.equals("A") || cat.equals("B") || cat.equals("C") => cat
      case _ => throw new NoSuchElementException(messages("cc.scheme.config.invalid.ni.category"))
    }
    getNiCategoryHelper(niCode, config.getConfigSeq("niCategories").get, None) match {
      case Some(z) => z
      case _ =>   throw new NoSuchElementException(messages("cc.scheme.config.ni.category.not.found"))
    }
  }

  //Scottish tax changes
  def getNiCategoryHelper(code: String, niCategories: Seq[Configuration], acc: Option[NiCategory]): Option[NiCategory] = {
    niCategories match {
      case Nil =>  acc
      case head :: tail =>
        if (head.get[String]("ni-cat-code").equals(code)) {
          val niCat = NiCategory(
            niCategoryCode = code,
            lelMonthlyLowerLimitForCat = head.get[Double]("LEL.monthly-lower-limit"),
            lelMonthlyUpperLimitForCat = head.get[Double]("LEL.monthly-upper-limit"),
            lelRateForCat = head.get[Double]("LEL.rate"),
            lelPtMonthlyLowerLimitForCat = head.get[Double]("LEL-PT.monthly-lower-limit"),
            lelPtMonthlyUpperLimitForCat = head.get[Double]("LEL-PT.monthly-upper-limit"),
            lelPtRateForCat = head.get[Double]("LEL-PT.rate"),
            ptUelMonthlyLowerLimitForCat = head.get[Double]("PT-UEL.monthly-lower-limit"),
            ptUelMonthlyUpperLimitForCat = head.get[Double]("PT-UEL.monthly-upper-limit"),
            ptUelRateForCat = head.get[Double]("PT-UEL.rate"),
            aboveUelMonthlyLowerLimitForCat = head.get[Double]("above-UEL.monthly-lower-limit"),
            aboveUelRateForCat = head.get[Double]("above-UEL.rate")
          )
          getNiCategoryHelper(code, Nil, Some(niCat))
        }
        else {
          getNiCategoryHelper(code, tail, acc)
        }
    }
  }
}

case class NiCategory(
          niCategoryCode: String,
          lelMonthlyLowerLimitForCat: Double,
          lelMonthlyUpperLimitForCat: Double,
          lelRateForCat: Double,
          lelPtMonthlyLowerLimitForCat: Double,
          lelPtMonthlyUpperLimitForCat: Double,
          lelPtRateForCat: Double,
          ptUelMonthlyLowerLimitForCat: Double,
          ptUelMonthlyUpperLimitForCat: Double,
          ptUelRateForCat: Double,
          aboveUelMonthlyLowerLimitForCat: Double,
          aboveUelRateForCat: Double)

case class ESCTaxYearConfig (
                             post2011MaxExemptionMonthlyBasic: Double,
                             post2011MaxExemptionMonthlyHigher: Double,
                             post2011MaxExemptionMonthlyAdditional: Double,
                             defaultTaxCode: String,
                             personalAllowanceRate: Double,
                             defaultPersonalAllowance: Double,
                             taxStarterRate: Double,
                             taxStarterBandCapacity: Double,
                             taxBasicRate: Double,
                             taxBasicBandCapacity: Double,
                             taxIntermediateRate: Double,
                             taxIntermediateBandCapacity: Double,
                             taxHigherRateBandCapacity: Double,
                             taxHigherRate: Double,
                             taxHigherBandUpperLimit: Double,
                             taxAdditionalRate: Double,
                             taxAdditionalBandLowerLimit: Double,
                             niLimit: Double,
                             niCategory: NiCategory,
                             basicNiThresholdUk: Double)
