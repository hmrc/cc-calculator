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

package models.output.tfc

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Writes}
import utils.CCFormat

/**
 * Created by user on 18/06/15.
 */
case class TFCCalculation(
                           householdContribution: Contribution,
                           numberOfPeriods: Short,
                           periods: List[TFCPeriod]
                           )

object TFCCalculation extends CCFormat {
  implicit val tfcCalculationWrites : Writes[TFCCalculation] = Json.writes[TFCCalculation]
}

case class TFCPeriod(
                      from: LocalDate,
                      until: LocalDate,
                      periodContribution : Contribution,
                      children: List[OutputChild]
                      )

object TFCPeriod extends CCFormat{
  implicit val periodWrites : Writes[TFCPeriod] = Json.writes[TFCPeriod]
}

case class OutputChild(
                        childCareCost : BigDecimal = BigDecimal(0.00),
                        childContribution : Contribution
                        )

object OutputChild extends CCFormat {
  implicit val childWrites : Writes[OutputChild] = Json.writes[OutputChild]
}

case class Contribution (
                          parent : BigDecimal = BigDecimal(0.00),
                          government : BigDecimal = BigDecimal(0.00),
                          totalChildCareSpend : BigDecimal = BigDecimal(0.00)
                          )

object Contribution {
  implicit val contributionWrites : Writes[Contribution] = Json.writes[Contribution]
}
