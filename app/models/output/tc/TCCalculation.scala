/*
 * Copyright 2016 HM Revenue & Customs
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
import play.api.libs.functional.syntax._
import play.api.libs.json.Writes._
import play.api.libs.json._

/**
 * Created by shridhaarchippaa on 18/06/15.
 */

object TCCalculation extends utils.CCFormat {
  implicit val TCCalculationWrites: Writes[TCCalculation] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "proRataEnd").writeNullable[LocalDate](jodaLocalDateWrites(datePattern)) and
          (JsPath \ "totalAwardAmount").write[BigDecimal] and
            (JsPath \ "totalAwardProRataAmount").write[BigDecimal] and
              (JsPath \ "houseHoldAdviceAmount").write[BigDecimal] and
               (JsPath \ "totalHouseHoldAdviceProRataAmount").write[BigDecimal] and
                  (JsPath \ "taxYears").write[List[TaxYear]]
    )(unlift(TCCalculation.unapply))
}

case class TCCalculation(from: LocalDate,
                         until: LocalDate,
                         proRataEnd: Option[LocalDate] = None,
                         totalAwardAmount: BigDecimal = 0.00,
                         totalAwardProRataAmount: BigDecimal = 0.00,
                         houseHoldAdviceAmount: BigDecimal = 0.00,
                         totalHouseHoldAdviceProRataAmount: BigDecimal = 0.00,
                         taxYears: List[TaxYear]
                          )

object TaxYear extends utils.CCFormat {
  implicit val TaxYearWrites: Writes[TaxYear] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "proRataEnd").writeNullable[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "taxYearAwardAmount").write[BigDecimal] and
          (JsPath \ "taxYearAwardProRataAmount").write[BigDecimal] and
            (JsPath \ "taxYearAdviceAmount").write[BigDecimal] and
      (JsPath \ "taxYearAdviceProRataAmount").write[BigDecimal] and
              (JsPath \ "periods").write[List[Period]]
    )(unlift(TaxYear.unapply))
}

case class TaxYear(
                    from: LocalDate,
                    until: LocalDate,
                    proRataEnd: Option[LocalDate] = None,
                    taxYearAwardAmount: BigDecimal = BigDecimal(0.00),
                    taxYearAwardProRataAmount: BigDecimal = BigDecimal(0.00),
                    taxYearAdviceAmount: BigDecimal = BigDecimal(0.00),
                    taxYearAdviceProRataAmount: BigDecimal = BigDecimal(0.00),
                    periods: List[Period]
                    )

object Period extends utils.CCFormat {
  implicit val ElementWrites: Writes[Period] = (
    (JsPath \ "from").write[LocalDate](jodaLocalDateWrites(datePattern)) and
      (JsPath \ "until").write[LocalDate](jodaLocalDateWrites(datePattern)) and
        (JsPath \ "periodNetAmount").write[BigDecimal] and
          (JsPath \ "periodAdviceAmount").write[BigDecimal] and
            (JsPath \ "elements").write[Elements]
    )(unlift(Period.unapply))
}

case class Period(from: LocalDate,
                  until: LocalDate,
                  periodNetAmount: BigDecimal = 0.00,
                  periodAdviceAmount: BigDecimal = 0.00,
                  elements: Elements)


object Elements {
  implicit val ElementsWrites: Writes[Elements] = (
      (JsPath \ "wtcWorkElement").write[Element] and
        (JsPath \ "wtcChildcareElement").write[Element] and
         (JsPath \ "ctcIndividualElement").write[Element] and
           (JsPath \ "ctcFamilyElement").write[Element]
    )(unlift(Elements.unapply))
}

case class Elements(wtcWorkElement: Element,
                     wtcChildcareElement: Element,
                     ctcIndividualElement: Element,
                     ctcFamilyElement: Element) {

  def wtcWorkElementNetDueIsNil = {
    wtcWorkElement.netAmount.equals(BigDecimal(0.00))
  }

}

object Element {
  implicit val ElementWrites: Writes[Element] = (
    (JsPath \ "netAmount").write[BigDecimal] and
      (JsPath \ "maximumAmount").write[BigDecimal] and
        (JsPath \ "taperAmount").write[BigDecimal]
    )(unlift(Element.unapply))
}

case class Element(netAmount: BigDecimal = 0.00,
                   maximumAmount: BigDecimal = 0.00,
                   taperAmount: BigDecimal = 0.00)
