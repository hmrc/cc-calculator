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

package models.input

import models.input.esc.ESCEligibility
import models.input.tc._
import models.input.tfc.TFCEligibility
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.util.Try

/**
 * Created by user on 18/06/15.
 */

object APIModels extends utils.CCFormat {


  case class Request(
                      payload: Payload
                      ) {
    def getTaxCreditsEligibility = {
      Try {
        payload.eligibility.tc.get
      }
    }

    def getESCEligibility = {
      Try {
        payload.eligibility.esc.get
      }
    }

    def getTFCEligibility = {
      Try{
        payload.eligibility.tfc.get
      }
    }
  }

  object Request {
    implicit val requestFormat: Reads[Request] =
      (JsPath \ "payload").read[Payload].map { payload => Request(payload) }
  }

  case class Payload(
                      eligibility: Eligibility
                      )

  object Payload {
    implicit val payloadFormat: Reads[Payload] =
      (JsPath \ "eligibility").read[Eligibility].map { eligibility => Payload(eligibility) }
  }

  case class Eligibility(
                          tc: Option[TCEligibility],
                          tfc: Option[TFCEligibility],
                          esc: Option[ESCEligibility]
                          )

  object Eligibility {
    implicit val eligibilityFormat: Reads[Eligibility] = (
      (JsPath \ "tc").readNullable[TCEligibility] and
        (JsPath \ "tfc").readNullable[TFCEligibility] and
        (JsPath \ "esc").readNullable[ESCEligibility]
      )(Eligibility.apply _)
  }



}
