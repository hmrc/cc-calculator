/*
 * Copyright 2019 HM Revenue & Customs
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
import play.api.Mode.Mode

case class TFCTaxYearConfig(
                             topUpPercent: Double,
                             maxEligibleChildcareAmount: Double,
                             maxEligibleChildcareAmountForDisabled: Double,
                             maxGovtContribution: Double,
                             maxGovtContributionForDisabled: Double
                             )

class TFCConfig @Inject()(val appConfig: AppConfig) extends CCConfig(appConfig) {

  val mode: Mode = appConfig.mode
  val runModeConfiguration: Configuration = appConfig.runModeConfiguration

  def getConfig(currentDate: LocalDate) : TFCTaxYearConfig  = {

    val configs: Seq[play.api.Configuration] = runModeConfiguration.getConfigSeq("tfc.rule-change").get

    // get the default config and keep
    val defaultConfig: Configuration =
      configs.find(_.getString("rule-date").contains("default")).head

    // fetch the config if it matches the particular year
    getConfigForTaxYear(currentDate, configs) match {
      case Some(x) => getTaxYear(x)
      case _ => getTaxYear(defaultConfig)
    }
  }

  def getTaxYear(config : Configuration): TFCTaxYearConfig = {
    TFCTaxYearConfig(
      topUpPercent = config.getDouble("top-up-percent").get,
      maxEligibleChildcareAmount = config.getDouble("max-eligible-child-care-amount-per-child").get,
      maxEligibleChildcareAmountForDisabled = config.getDouble("max-eligible-child-care-amount-per-disabled-child").get,
      maxGovtContribution = config.getDouble("max-government-contribution-per-child").get,
      maxGovtContributionForDisabled = config.getDouble("max-government-contribution-per-disabled-child").get
    )
  }

}
