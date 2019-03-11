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

package models.output.tfc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import play.api.libs.json.JodaWrites._
import play.api.libs.json.JodaReads._

/**
 * Created by user on 18/06/15.
 */
case class TFCCalculatorOutput(
                                householdContribution: TFCContribution,
                                numberOfPeriods: Short,
                                periods: List[TFCPeriod]
                           )

object TFCCalculatorOutput {
  implicit val tfcCalculationWrites : Writes[TFCCalculatorOutput] = Json.writes[TFCCalculatorOutput]
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodContribution : TFCContribution,
                      children: List[TFCOutputChild]
                      )

object TFCPeriod {
  implicit val periodWrites : Writes[TFCPeriod] = Json.writes[TFCPeriod]
}

case class TFCOutputChild(
                        childCareCost : BigDecimal = BigDecimal(0.00),
                        childContribution : TFCContribution
                        )

object TFCOutputChild {
  implicit val childWrites : Writes[TFCOutputChild] = Json.writes[TFCOutputChild]
}

case class TFCContribution(
                          parent : BigDecimal = BigDecimal(0.00),
                          government : BigDecimal = BigDecimal(0.00),
                          totalChildCareSpend : BigDecimal = BigDecimal(0.00)
                          )

object TFCContribution {
  implicit val contributionWrites : Writes[TFCContribution] = Json.writes[TFCContribution]
}
