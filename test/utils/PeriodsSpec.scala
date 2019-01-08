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

import play.api.libs.json.{JsError, JsString, Json}

/**
 * Created by adamconder on 09/06/15.
 */
class PeriodsSpec extends FakeCCCalculatorApplication {

  "Periods" should {

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
  "Enumutils" should {
    "return JsError" in {
      class test extends Enumeration

      val utilRes = EnumUtils.enumReads(new test).reads(Json.obj("periods" -> "0"))

      utilRes shouldBe JsError("String value expected")
    }
  }
}
