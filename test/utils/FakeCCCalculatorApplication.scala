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

import java.nio.charset.Charset

import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.scalatest.Suite
import play.api.{Application, Configuration}
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{MessagesControllerComponents, Result}
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.{ExecutionContext, Future}

trait FakeCCCalculatorApplication extends PlaySpec {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens"                              -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250"
  )

  val formatter: DateTimeFormatter       = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  def parseDate(date: String): LocalDate = LocalDate.parse(date, formatter)

  lazy val app: Application =
    new GuiceApplicationBuilder()
      .configure(Configuration("metrics.enabled" -> false))
      .configure(Configuration("metrics.jvm" -> false))
      .configure(config)
      .build()

  given Materializer                      = app.materializer
  given Lang                              = Lang("en")
  given messages: MessagesApi             = app.injector.instanceOf[MessagesApi]
  given mcc: MessagesControllerComponents = app.injector.instanceOf[MessagesControllerComponents]
  given ExecutionContext                  = app.injector.instanceOf[ExecutionContext]

  def jsonBodyOf(result: Result)(using Materializer): JsValue =
    Json.parse(bodyOf(result))

  def jsonBodyOf(resultF: Future[Result])(using Materializer): Future[JsValue] =
    resultF.map(jsonBodyOf)

  def bodyOf(result: Result)(using Materializer): String = {
    val bodyBytes: ByteString = await(result.body.consumeData)
    // We use the default charset to preserve the behaviour of a previous
    // version of this code, which used new String(Array[Byte]).
    // If the fact that the previous version used the default charset was an
    // accident then it may be better to decode in UTF-8 or the charset
    // specified by the result's headers.
    bodyBytes.decodeString(Charset.defaultCharset().name)
  }

  def bodyOf(resultF: Future[Result])(using Materializer): Future[String] =
    resultF.map(bodyOf)

  import scala.concurrent.duration.*
  import scala.concurrent.{Await, Future}

  given FiniteDuration = 5.seconds

  given extractAwait[A]: Conversion[Future[A], A] =
    (future: Future[A]) => await[A](future)

  def await[A](future: Future[A])(using timeout: Duration): A = Await.result(future, timeout)

  // Convenience to avoid having to wrap andThen() parameters in Future.successful
  given liftFuture[A]: Conversion[A, Future[A]] =
    value => Future.successful(value)

  def status(of: Result): Int = of.header.status

  def status(of: Future[Result])(using timeout: Duration): Int = status(Await.result(of, timeout))

}
