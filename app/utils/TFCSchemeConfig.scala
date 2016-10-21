/*
 * Copyright 2016 HM Revenue & Customs
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

import org.joda.time.LocalDate
import play.api.Play._
import play.api.{Configuration, Play}
import uk.gov.hmrc.play.config.ServicesConfig

/**
 * Created by user on 26/01/16.
 */

trait TFCConfig {
  val defaultMaxNoOfChildren = 25
  lazy val maxNoOfChildren = configuration.getInt(s"tfc.max-no-of-children").getOrElse(defaultMaxNoOfChildren)
  val defaultMaxNameLength = 25
  lazy val maxNameLength = configuration.getInt(s"tfc.max-name-length").getOrElse(defaultMaxNameLength)

  lazy val taxYearEndMonth = configuration.getInt(s"tfc.end-of-tax-year-date.month").getOrElse(0)
  lazy val taxYearEndDay = configuration.getInt(s"tfc.end-of-tax-year-date.day").getOrElse(0)

}

case class TFCTaxYearConfig(
                             topUpPercent: Double,
                             maxEligibleChildcareAmount: Double,
                             maxEligibleChildcareAmountForDisabled: Double,
                             maxGovtContribution: Double,
                             maxGovtContributionForDisabled: Double
                             )

object TFCConfig extends CCConfig with TFCConfig with ServicesConfig {

  def getConfig(currentDate: LocalDate) : TFCTaxYearConfig  = {

    val configs: Seq[play.api.Configuration] = Play.application.configuration.getConfigSeq("tfc.rule-change").get
    // get the default config and keep
    val defaultConfig = configs.filter(x => {
      x.getString("rule-date").equals(Some("default"))
    }).head
    // fetch the config if it matches the particular year
    val result = getConfigForTaxYear(currentDate, configs)

    val config : TFCTaxYearConfig = result match {
      case Some(x) =>
        getTaxYear(x)
      case _ =>
        getTaxYear(defaultConfig)
    }
    config
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
