/*
 * Copyright 2024 HM Revenue & Customs
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
import java.time.LocalDate
import play.api.Configuration
import play.api.i18n.{Lang, MessagesApi}
import scala.collection.JavaConverters.asScalaBufferConverter

class ESCConfig @Inject()(appConfig: AppConfig,
                          configuration: Configuration,
                          messages: MessagesApi) extends CCConfig(appConfig) with MessagesObject {

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

    val localConfig = config.getOptional[Configuration](s"tax.${location.toLowerCase}").getOrElse(config.getOptional[Configuration]("tax.default").get)

    ESCTaxYearConfig(
      post2011MaxExemptionMonthlyBasic = config.getOptional[Double]("post-2011-maximum-exemption.basic.monthly").get,
      post2011MaxExemptionMonthlyHigher = config.getOptional[Double]("post-2011-maximum-exemption.higher.monthly").get,
      post2011MaxExemptionMonthlyAdditional = config.getOptional[Double]("post-2011-maximum-exemption.additional.monthly").get,
      defaultTaxCode = localConfig.getOptional[String]("default-tax-code").get,
      personalAllowanceRate = localConfig.getOptional[Double]("personal-allowance.rate").get,
      defaultPersonalAllowance = localConfig.getOptional[Double]("personal-allowance.default-personal-allowance").get,
      taxStarterRate = localConfig.getOptional[Double]("starter.rate").getOrElse(0),
      taxStarterBandCapacity = localConfig.getOptional[Double]("starter.band-capacity-annual-amount").getOrElse(0),
      taxBasicRate = localConfig.getOptional[Double]("basic.rate").get,
      taxBasicBandCapacity = localConfig.getOptional[Double]("basic.band-capacity-annual-amount").get,
      taxIntermediateRate = localConfig.getOptional[Double]("intermediate.rate").getOrElse(0),
      taxIntermediateBandCapacity = localConfig.getOptional[Double]("intermediate.band-capacity-annual-amount").getOrElse(0),
      taxHigherRateBandCapacity = localConfig.getOptional[Double]("higher.band-capacity-annual-amount").getOrElse(0),
      taxHigherRate = localConfig.getOptional[Double]("higher.rate").get,
      taxHigherBandUpperLimit = localConfig.getOptional[Double]("higher.band-annual-upper-limit").getOrElse(0),
      taxAdditionalRate = localConfig.getOptional[Double]("additional.rate").get,
      taxAdditionalBandLowerLimit = localConfig.getOptional[Double]("additional.band-annual-lower-limit").get,
      niLimit = config.getOptional[Double]("ni-limit").get,
      niCategory = niCat,
      basicNiThresholdUk = localConfig.getOptional[Double]("basic-ni-threshold-uk").getOrElse(0)
    )
  }

  def getNiCategory(niCategoryCode: String, config: Configuration): NiCategory ={
    // get the ni Category
    val niCode = niCategoryCode match {
      case cat if cat.isEmpty => config.get[String]("default-ni-code")
      case cat if cat.equals("A") || cat.equals("B") || cat.equals("C") => cat
      case _ => throw new NoSuchElementException(messages("cc.scheme.config.invalid.ni.category"))
    }
    val configs : Seq[play.api.Configuration] = config.underlying.getConfigList("niCategories")
      .asScala.map(Configuration(_)).toSeq
    getNiCategoryHelper(niCode, configs, None) match {
      case Some(z) => z
      case _ =>   throw new NoSuchElementException(messages("cc.scheme.config.ni.category.not.found"))
    }
  }

  //Scottish tax changes
  def getNiCategoryHelper(code: String, niCategories: Seq[Configuration], acc: Option[NiCategory]): Option[NiCategory] = {
    niCategories.toList match {
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
