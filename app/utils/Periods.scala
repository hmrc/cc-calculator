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

import play.api.Logging
import play.api.libs.json._

object EnumUtils extends Logging {

  def enumReads[E <: Enumeration](`enum`: E): Reads[E#Value] =
    new Reads[E#Value] {
      def reads(json: JsValue): JsResult[E#Value] = json match {
        case JsString(s) => JsSuccess(enum.withName(s))
        case _ =>
          logger.warn(s"EnumUtils.enumReads - JsError::: String value expected")
          JsError("String value expected")
      }
    }

  implicit def enumWrites[E <: Enumeration]: Writes[E#Value] =
    new Writes[E#Value] {
      def writes(v: E#Value): JsValue = JsString(v.toString)
    }

}

object Periods extends Enumeration {
  type Period = Value

  private val yearlyIndex  = 4
  private val invalidIndex = 5

  val Weekly  = Value(0, "Week")
  val Monthly = Value(2, "Month")
  val Yearly  = Value(yearlyIndex, "Year")
  val INVALID = Value(invalidIndex, "INVALID")

  implicit val enumReads: Reads[Period] = EnumUtils.enumReads(Periods)

  implicit def enumWrites: Writes[Period] = EnumUtils.enumWrites

  def toString(period: Value): String =
    period match {
      case Weekly  => "cc.period.weekly"
      case Monthly => "cc.period.monthly"
      case Yearly  => "cc.period.yearly"
      case _       => "cc.period.invalid"
    }

  def toPeriod(period: String): Value =
    period.toLowerCase match {
      case "monthly" => Monthly
      case _         => INVALID
    }

}
