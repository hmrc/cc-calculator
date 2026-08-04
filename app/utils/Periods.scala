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
import play.api.libs.json.*

object EnumUtils extends Logging {

  def enumReads[E](values: Array[E])(asString: E => String): Reads[E] =
    Reads {
      case JsString(value) =>
        values.find(enumValue => asString(enumValue) == value) match {
          case Some(enumValue) =>
            JsSuccess(enumValue)

          case None =>
            logger.warn(s"EnumUtils.enumReads - Unknown enum value: $value")
            JsError(s"Unknown enum value: $value")
        }

      case _ =>
        logger.warn("EnumUtils.enumReads - String value expected")
        JsError("String value expected")
    }

  def enumWrites[E](asString: E => String): Writes[E] =
    Writes(enumValue => JsString(asString(enumValue)))

}

enum Periods(val id: Int, val jsonValue: String) {

  case Weekly extends Periods(id = 0, jsonValue = "Week")

  case Monthly extends Periods(id = 2, jsonValue = "Month")

  case Yearly extends Periods(id = 4, jsonValue = "Year")

  case INVALID extends Periods(id = 5, jsonValue = "INVALID")

  override def toString: String = jsonValue
}

object Periods {

  type Period = Periods

  given enumReads: Reads[Period] = EnumUtils.enumReads(Periods.values)(_.jsonValue)

  given enumWrites: Writes[Period] = EnumUtils.enumWrites(_.jsonValue)

  def toString(period: Period): String =
    period match {
      case Weekly  => "cc.period.weekly"
      case Monthly => "cc.period.monthly"
      case Yearly  => "cc.period.yearly"
      case INVALID => "cc.period.invalid"
    }

  def toPeriod(period: String): Period =
    period.toLowerCase match {
      case "monthly" => Monthly
      case _         => INVALID
    }

}
