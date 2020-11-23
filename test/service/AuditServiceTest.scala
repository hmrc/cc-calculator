/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.{ForwardedFor, SessionId}
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.test.UnitSpec

import scala.concurrent.{ExecutionContext, Future}

class AuditServiceTest extends UnitSpec {

  "AuditService" when {

    "auditer should send message" in {

      implicit val request = FakeRequest()

      implicit var hc = new HeaderCarrier(forwarded = Some(ForwardedFor("testIp")),  // test the IP address is in audit request
        sessionId = Some(SessionId("sessionid-random")))

      val auditConnectorObj = new AuditConnector {

        var lastAuditEvent : Option[DataEvent]  = None

        override def auditingConfig: AuditingConfig = ???
        override def sendEvent(event: DataEvent)(implicit hc: HeaderCarrier = HeaderCarrier(), ec : ExecutionContext): Future[AuditResult] = {
          lastAuditEvent = Some(event.asInstanceOf[DataEvent])
          Future.successful(AuditResult.Success)
        }
      }

      val auditTest: AuditService = new AuditService(auditConnectorObj) {
        override val auditSource = "cc-eligibility"
      }

      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))(hc)


      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))

      val auditEvent : DataEvent = auditConnectorObj.lastAuditEvent.get

      auditEvent should not equal Nil

      auditEvent.auditSource should equal("cc-eligibility")
      auditEvent.auditType should equal("testTranType")
      auditEvent.detail("randomDetails") should equal("+=+=+=+=+=+=+=+=+")
      auditEvent.tags.get("x-forwarded-for") shouldBe Some("testIp")
    }

  }

}
