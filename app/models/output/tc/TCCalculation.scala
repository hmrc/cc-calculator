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

package models.output.tc

import org.joda.time.LocalDate
import play.api.libs.json._

object TCCalculation {
  implicit val TCCalculationWrites: Writes[TCCalculation] = Json.writes[TCCalculation]
}

case class TCCalculation(from: LocalDate,
                         until: LocalDate,
                         totalAwardAmount: BigDecimal = 0.00,
                         houseHoldAdviceAmount: BigDecimal = 0.00,
                         taxYears: List[TaxYear]
                        )

object TaxYear {
  implicit val TaxYearWrites: Writes[TaxYear] = Json.writes[TaxYear]
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    taxYearAwardAmount: BigDecimal = BigDecimal(0.00),
                    taxYearAdviceAmount: BigDecimal = BigDecimal(0.00),
                    periods: List[Period]
                  )

object Period {
  implicit val ElementWrites: Writes[Period] = Json.writes[Period]
}

case class Period(from: LocalDate,
                  until: LocalDate,
                  periodNetAmount: BigDecimal = 0.00,
                  periodAdviceAmount: BigDecimal = 0.00,
                  elements: Elements)

object Elements {
  implicit val ElementsWrites: Writes[Elements] = Json.writes[Elements]
}

case class Elements(wtcWorkElement: Element,
                    wtcChildcareElement: Element,
                    ctcIndividualElement: Element,
                    ctcFamilyElement: Element)

object Element {
  implicit val ElementWrites: Writes[Element] = Json.writes[Element]
}

case class Element(netAmount: BigDecimal = 0.00,
                   maximumAmount: BigDecimal = 0.00,
                   taperAmount: BigDecimal = 0.00)
