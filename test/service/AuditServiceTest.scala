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

package service

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.play.PlaySpec
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.{ForwardedFor, HeaderCarrier, SessionId}
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditChannel, AuditConnector, AuditResult, DatastreamMetrics}
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.{ExecutionContext, Future}

class AuditServiceTest extends PlaySpec {

  "AuditService" when {

    "auditer should send message" in {

      implicit val hc = HeaderCarrier(
        forwarded = Some(ForwardedFor("testIp")), // test the IP address is in audit request
        sessionId = Some(SessionId("sessionid-random"))
      )

      implicit val ec = new GuiceApplicationBuilder().build().injector.instanceOf[ExecutionContext]

      val auditConnectorObj = new AuditConnector {

        var lastAuditEvent: Option[DataEvent] = None

        override def sendEvent(
            event: DataEvent
        )(implicit hc: HeaderCarrier = HeaderCarrier(), ec: ExecutionContext): Future[AuditResult] = {
          lastAuditEvent = Some(event.asInstanceOf[DataEvent])
          Future.successful(AuditResult.Success)
        }

        override def auditingConfig: AuditingConfig = ???

        override def auditChannel: AuditChannel = ???

        override def datastreamMetrics: DatastreamMetrics = ???
      }

      val auditTest: AuditService = new AuditService(auditConnectorObj) {
        override val auditSource = "cc-eligibility"
      }

      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))(hc, ec)

      auditTest.sendEvent("testTranType", Map("randomDetails" -> "+=+=+=+=+=+=+=+=+"))

      val auditEvent: DataEvent = auditConnectorObj.lastAuditEvent.get

      (auditEvent should not).equal(Nil)

      auditEvent.auditSource must equal("cc-eligibility")
      auditEvent.auditType must equal("testTranType")
      auditEvent.detail("randomDetails") must equal("+=+=+=+=+=+=+=+=+")
      auditEvent.tags.get("x-forwarded-for") shouldBe Some("testIp")
    }

  }

}
