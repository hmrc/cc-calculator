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

import org.scalatest.matchers.should.Matchers.shouldBe
import play.api.libs.json.{JsError, JsString, JsSuccess, Json}

/** Created by adamconder on 09/06/15.
  */
class PeriodsSpec extends FakeCCCalculatorApplication {

  private enum TestEnum {
    case One, Two
  }

  "Periods" must {

    "convert Periods.Monthly to Json" in {
      val monthly = Periods.Monthly
      Json.toJson(monthly) shouldBe JsString("Month")
    }

    "convert to Periods.Monthly" in {
      val monthly = "monthly"
      Periods.toPeriod(monthly) shouldBe Periods.Monthly
    }

    "return INVALID for an incorrect Period" in {
      val invalid = "invalid"
      Periods.toPeriod(invalid) shouldBe Periods.INVALID
    }

    "convert Weekly to string" in {
      val weekly = Periods.Weekly
      Periods.toString(weekly) shouldBe messages("cc.period.weekly")
    }

    "convert Monthly to string" in {
      val monthly = Periods.Monthly
      Periods.toString(monthly) shouldBe messages("cc.period.monthly")
    }

    "convert Yearly to string" in {
      val yearly = Periods.Yearly
      Periods.toString(yearly) shouldBe messages("cc.period.yearly")
    }

    "convert INVALID to string" in {
      val invalid = Periods.INVALID
      Periods.toString(invalid) shouldBe messages("cc.period.invalid")
    }

  }

  "EnumUtils" must {

    "return the enum value for a valid string" in {
      val result = EnumUtils.enumReads(TestEnum.values)(_.toString).reads(JsString("One"))

      result shouldBe JsSuccess(TestEnum.One)
    }

    "return JsError for an invalid enum string" in {
      val result =
        EnumUtils.enumReads(TestEnum.values)(_.toString).reads(JsString("Three"))

      result shouldBe JsError("Unknown enum value: Three")
    }

    "return JsError when the JSON value is not a string" in {

      val utilRes = EnumUtils.enumReads(TestEnum.values)(_.toString).reads(Json.obj("periods" -> "0"))

      utilRes shouldBe JsError("String value expected")
    }
  }

}
