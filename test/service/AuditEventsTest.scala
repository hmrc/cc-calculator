/*
 * Copyright 2021 HM Revenue & Customs
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

import akka.stream.Materializer
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.mockito.MockitoSugar
import play.api.test.FakeRequest
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.audit.http.connector.{AuditConnector, AuditResult}
import uk.gov.hmrc.play.audit.model.DataEvent
import org.scalatestplus.play.PlaySpec
import play.api.inject.ApplicationLifecycle
import utils.FakeCCCalculatorApplication

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

class AuditEventsTest extends PlaySpec with FakeCCCalculatorApplication with MockitoSugar {
  implicit val request = FakeRequest()
  implicit var hc = new HeaderCarrier()

  trait ObservableAuditConnector extends AuditConnector {
    var events : ListBuffer[DataEvent] = new ListBuffer[DataEvent]

    def observedEvents : ListBuffer[DataEvent] = events

    def addEvent(auditEvent : DataEvent): Unit = {
      events = events += auditEvent
    }

    override def auditingConfig: AuditingConfig = ???
    override def sendEvent(event: DataEvent)(implicit hc: HeaderCarrier = HeaderCarrier(), ec : ExecutionContext): Future[AuditResult] = {
      addEvent(event.asInstanceOf[DataEvent])
      Future.successful(AuditResult.Success)
    }
  }

  def createObservableAuditConnector = new ObservableAuditConnector{
    override def materializer: Materializer = ???
    override def lifecycle: ApplicationLifecycle = ???
  }

  def createAuditor(observableAuditConnector : ObservableAuditConnector) = {

    val testAuditService = new AuditService(observableAuditConnector) {
      override val auditSource = "cc-calculator"
    }

    new AuditEvents(testAuditService)
  }

  "Audit Events" must {

    "audit request received - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("Request")
      event.detail("data") should startWith("Data")

    }

    "audit request received for TFC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("TFCRequest")
      event.detail("data") should startWith("Data")

    }

    "audit response processed for TFC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTFCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("TFCResponse")
      event.detail("data") should startWith("Data")

    }

    "audit request received for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("TCRequest")
      event.detail("data") should startWith("Data")

    }

    "audit response processed for TC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditTCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("TCResponse")
      event.detail("data") should startWith("Data")

    }

    "audit request received for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCRequest("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("ESCRequest")
      event.detail("data") should startWith("Data")

    }

    "audit response processed for ESC - success " in {

      val observableAuditConnector = createObservableAuditConnector
      val auditor = createAuditor(observableAuditConnector)

      auditor.auditESCResponse("Data")

      val event =  observableAuditConnector.events.head

      event.auditType must equal("ESCResponse")
      event.detail("data") should startWith("Data")

    }
  }


}
