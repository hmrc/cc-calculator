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

package config

import com.typesafe.config.Config

import play.api.Mode.Mode
import play.api.{Configuration, Play}
import uk.gov.hmrc.http._

import uk.gov.hmrc.play.http.ws._

import akka.actor.ActorSystem

trait RunModeConfig {
  def appNameConfiguration: Configuration = Play.current.configuration
  def runModeConfiguration: Configuration = Play.current.configuration
  def mode: Mode = Play.current.mode
}

object WSHttp extends HttpGet with WSGet with HttpPut with WSPut with HttpPost with WSPost with HttpDelete with WSDelete with HttpPatch with WSPatch {
  override val hooks = NoneRequired

  override val configuration: Option[Config] = Some(Play.current.configuration.underlying)

  override protected def actorSystem: ActorSystem = Play.current.actorSystem
}
