/*
 * Copyright 2023 HM Revenue & Customs
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

package service

import javax.inject.Inject
import uk.gov.hmrc.http.{HeaderCarrier, HeaderNames}
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class AuditService @Inject()(val auditConnector: AuditConnector) {

  val auditSource : String = "cc-calculator"

  def sendEvent(auditType:String, details: Map[String, String], sessionId: Option[String] = None)
               (implicit hc: HeaderCarrier): Future[AuditResult] = {
    auditConnector.sendEvent(buildEvent(auditType, details, sessionId))
  }

  def buildEvent(auditType:String, details: Map[String, String], sessionId: Option[String] = None)
                (implicit hc: HeaderCarrier): DataEvent = {
    val auditEvent = DataEvent(
      auditSource =  auditSource,
      auditType = auditType,
      tags = hc.headers(HeaderNames.explicitlyIncludedHeaders).toMap,
      detail = details)
//      detail = generateDetails(request, details))
    auditEvent
  }

//  private def generateDetails(request: Request[_], details: Map[String, String]): Map[String, String] = {
//    details ++ Map("deviceID" -> DeviceId(request).map(_.id).getOrElse("-"))
//  }

}
