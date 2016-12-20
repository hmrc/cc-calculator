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

package utils

import models.input.APIModels.Request
import play.api.Logger
import play.api.libs.json.{JsError, JsResult, JsSuccess}

/**
 * Created by adamconder on 22/06/15.
 */
trait CCJsonLogger {

  def logResult(result: JsResult[Request]) = {
    result match {
      case JsSuccess(x, _) =>
        Logger.info(s"\n\n JsSuccess json \n\n")
      case e: JsError =>
        val json = JsError.toJson(e)
        Logger.warn(s"\n\n JsError: ${json.toString()}\n\n")
    }
  }

}
