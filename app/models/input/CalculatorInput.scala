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

package models.input

import models.input.esc.ESCCalculatorInput
import models.input.tc.TCCalculatorInput
import models.input.tfc.TFCCalculatorInput
import play.api.libs.json.{Json, Reads}

case class CalculatorInput(tc: Option[TCCalculatorInput], tfc: Option[TFCCalculatorInput], esc: Option[ESCCalculatorInput])

object CalculatorInput {
  implicit val calculatorInput: Reads[CalculatorInput] = Json.reads[CalculatorInput]
}
