/*
 * Copyright 2023 HM Revenue & Customs
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
import akka.stream.Materializer
import akka.util.ByteString
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.Suite
import play.api.{Application, Configuration}
import play.api.i18n.{Lang, MessagesApi}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.mvc.{MessagesControllerComponents, Result}
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

trait FakeCCCalculatorApplication extends PlaySpec {
  this: Suite =>

  val config: Map[String, _] = Map(
    "csrf.sign.tokens" -> false,
    "govuk-tax.Test.services.contact-frontend.host" -> "localhost",
    "govuk-tax.Test.services.contact-frontend.port" -> "9250"
  )

  val formatter = DateTimeFormat.forPattern("yyyy-MM-dd")
  def parseDate(date: String): LocalDate = LocalDate.parse(date, formatter)

  /*lazy val app: Application = new GuiceApplicationBuilder()
    .configure(config)
    .build()*/

  lazy val app: Application =
    new GuiceApplicationBuilder()
      .disable[com.kenshoo.play.metrics.PlayModule]
      .disable[com.kenshoo.play.metrics.MetricsController]
      .configure(Configuration("metrics.enabled" -> false))
      .configure(Configuration("metrics.jvm" -> false))
      .configure(config)
      .build()


  implicit lazy val mat: Materializer = app.materializer
  implicit val lang: Lang = Lang("en")
  implicit lazy val messages: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit lazy val mcc: MessagesControllerComponents = app.injector.instanceOf[MessagesControllerComponents]

  def jsonBodyOf(result: Result)(implicit mat: Materializer): JsValue = {
    Json.parse(bodyOf(result))
  }

  def jsonBodyOf(resultF: Future[Result])(implicit mat: Materializer): Future[JsValue] = {
    resultF.map(jsonBodyOf)
  }

  def bodyOf(result: Result)(implicit mat: Materializer): String = {
    val bodyBytes: ByteString = await(result.body.consumeData)
    // We use the default charset to preserve the behaviour of a previous
    // version of this code, which used new String(Array[Byte]).
    // If the fact that the previous version used the default charset was an
    // accident then it may be better to decode in UTF-8 or the charset
    // specified by the result's headers.
    bodyBytes.decodeString(Charset.defaultCharset().name)
  }

  def bodyOf(resultF: Future[Result])(implicit mat: Materializer): Future[String] = {
    resultF.map(bodyOf)
  }

  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  implicit val defaultTimeout: FiniteDuration = 5 seconds

  implicit def extractAwait[A](future: Future[A]): A = await[A](future)

  def await[A](future: Future[A])(implicit timeout: Duration): A = Await.result(future, timeout)

  // Convenience to avoid having to wrap andThen() parameters in Future.successful
  implicit def liftFuture[A](v: A): Future[A] = Future.successful(v)

  def status(of: Result): Int = of.header.status

  def status(of: Future[Result])(implicit timeout: Duration): Int = status(Await.result(of, timeout))

}
