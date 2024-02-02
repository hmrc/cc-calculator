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

package models.utility

import play.api.libs.json.Json

/**
 * Created by adamconder on 28/09/15.
 */

// CalculationTaxBands used for determining monthly relief

case class CalculationTaxBands(
                                additionalRateBand : BigDecimal = BigDecimal(0.00),
                                higherRateBand : BigDecimal = BigDecimal(0.00),
                                basicRateBand : BigDecimal = BigDecimal(0.00),
                                intermediateRateBand: BigDecimal = BigDecimal(0.00),
                                starterRateBand: BigDecimal = BigDecimal(0.00),
                                zeroRateBand : BigDecimal = BigDecimal(0.00)
                                )
object CalculationTaxBands {
  implicit val taxBanksFormat = Json.format[CalculationTaxBands]
}

case class CalculationNIBands (
                              aboveUpperEarningsBand : BigDecimal = BigDecimal(0.00),
                              upperEarningsBand : BigDecimal = BigDecimal(0.00),
                              primaryEarningsBand : BigDecimal = BigDecimal(0.00),
                              lowerEarningsBand : BigDecimal = BigDecimal(0.00)
                                )

object CalculationNIBands {
  implicit val niBanksFormat = Json.format[CalculationNIBands]
}
