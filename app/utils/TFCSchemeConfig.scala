/*
 * Copyright 2022 HM Revenue & Customs
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

case class TFCTaxYearConfig(
                             topUpPercent: Double,
                             maxGovtContribution: Double,
                             maxGovtContributionForDisabled: Double
                             )

class TFCConfig @Inject()(val config: AppConfig,
                          configuration: Configuration) extends CCConfig(config) {

  def getConfig(currentDate: LocalDate) : TFCTaxYearConfig  = {

    val configs: Seq[play.api.Configuration] = configuration.get[Seq[Configuration]]("tfc.rule-change")

    // get the default config and keep
    val defaultConfig: Configuration =
      configs.find(_.get[String]("rule-date").contains("default")).head

    // fetch the config if it matches the particular year
    getConfigForTaxYear(currentDate, configs) match {
      case Some(x) => getTaxYear(x)
      case _ => getTaxYear(defaultConfig)
    }
  }

  def getTaxYear(config : Configuration): TFCTaxYearConfig = {
    TFCTaxYearConfig(
      topUpPercent = config.get[Double]("top-up-percent"),
      maxGovtContribution = config.get[Double]("max-government-contribution-per-child"),
      maxGovtContributionForDisabled = config.get[Double]("max-government-contribution-per-disabled-child")
    )
  }

}
