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

import javax.inject.Inject
import play.api.Mode.Mode
import play.api.{Configuration, Environment}
import uk.gov.hmrc.play.config.ServicesConfig

class AppConfig @Inject()(val runModeConfiguration: Configuration,
                          environment: Environment) extends ServicesConfig {
  val mode: Mode = environment.mode

  val defaultMaxNoOfChildren = 25
  val defaultMaxNameLength = 25
  lazy val maxNoOfChildren: Int = getInt("tfc.max-no-of-children")
  lazy val maxNameLength: Int = getInt("tfc.max-name-length")
  lazy val taxYearEndMonth: Int = getInt("tfc.end-of-tax-year-date.month")
  lazy val taxYearEndDay: Int = getInt("tfc.end-of-tax-year-date.day")

}


