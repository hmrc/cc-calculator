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

package models.output

import models.output.esc.ESCCalculation
import models.output.tc.TCCalculation
import models.output.tfc.TFCCalculation
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
 * Created by shridhaarchippaa on 22/06/15.
 */

object OutputAPIModel {

  case class AwardPeriod(
                          tc: Option[TCCalculation] = None,
                          tfc: Option[TFCCalculation] = None,
                          esc: Option[ESCCalculation] = None
                          )

  object AwardPeriod {
    implicit val AwardPeriodWrites: Writes[AwardPeriod] = (
      (JsPath \ "tc").write[Option[TCCalculation]] and
        (JsPath \ "tfc").write[Option[TFCCalculation]] and
          (JsPath \ "esc").write[Option[ESCCalculation]]
      )(unlift(AwardPeriod.unapply))  }


  case class Response(
                       awardPeriod: AwardPeriod
                       )

  object Response {
    implicit val ResponseWrites: Writes[Response] = (JsPath \ "awardPeriod").write[AwardPeriod].contramap { (response: Response) => response.awardPeriod }
  }

}
