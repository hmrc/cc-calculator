/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.play.test.UnitSpec

trait FakeCCCalculatorApplication extends UnitSpec with OneAppPerSuite {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250"
  )

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  def parseDate(date: String): LocalDate = LocalDate.parse(date, formatter)

  implicit override lazy val app: Application = new GuiceApplicationBuilder()
    .configure(config)
    .build()

  implicit lazy val mat: Materializer = app.materializer
  implicit val lang: Lang = Lang("en")
  implicit lazy val messages: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit lazy val mcc: MessagesControllerComponents = app.injector.instanceOf[MessagesControllerComponents]
}
