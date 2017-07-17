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

import akka.stream.Materializer
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.Suite
import org.scalatestplus.play.OneAppPerSuite
import play.api.Application
import play.api.i18n.Messages
import play.api.i18n.Messages.Implicits._
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.play.test.UnitSpec

trait FakeCCCalculatorApplication extends UnitSpec with OneAppPerSuite {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250" //,
//    "tc.rule-change.0.rule-date" -> "default",
//    "tc.rule-change.0.other-adjustment" -> 300,
//    "tc.rule-change.0.current-income-fall-difference-amount" -> 2500,
//    "tc.rule-change.0.current-income-rise-difference-amount" -> 2500,
//    "tc.rule-change.0.input-elements.wtc.basic-element" -> 1960,
//    "tc.rule-change.0.input-elements.wtc.second-adult-element" -> 2010,
//    "tc.rule-change.0.input-elements.wtc.lone-parent-element" -> 2010,
//    "tc.rule-change.0.input-elements.wtc.30-hour-element" -> 810,
//    "tc.rule-change.0.input-elements.wtc.disabled-worker-element" -> 3000,
//    "tc.rule-change.0.input-elements.wtc.severe-disabled-worker-element" -> 1290,
//    "tc.rule-change.0.input-elements.wtc.max-childcare-element-one-child" -> 175,
//    "tc.rule-change.0.input-elements.wtc.max-childcare-element-more-children" -> 300,
//    "tc.rule-change.0.input-elements.wtc.percent-of-eligible-cost-covered" -> 70,
//    "tc.rule-change.0.input-elements.ctc.child-element" -> 2780,
//    "tc.rule-change.0.input-elements.ctc.young-person-element" -> 2780,
//    "tc.rule-change.0.input-elements.ctc.disabled-child-element" -> 3175,
//    "tc.rule-change.0.input-elements.ctc.severe-disabled-child-element" -> 1290,
//    "tc.rule-change.0.input-elements.ctc.family-element" -> 545,
//    "tc.rule-change.0.thresholds.wtc-income-threshold" -> 6420,
//    "tc.rule-change.0.thresholds.ctc-income-threshold" -> 16105,
//    "tc.rule-change.0.thresholds.percent-of-taper-rate" -> 41
  )

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  def parseDate(date: String): LocalDate = LocalDate.parse(date, formatter)

  implicit override lazy val app: Application = new GuiceApplicationBuilder()
    .configure(config)
    .build()

  implicit lazy val mat: Materializer = app.materializer
  implicit lazy val messages: Messages = applicationMessages
}
