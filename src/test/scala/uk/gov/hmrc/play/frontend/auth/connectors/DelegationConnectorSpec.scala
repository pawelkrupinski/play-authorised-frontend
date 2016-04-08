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

package uk.gov.hmrc.play.frontend.auth.connectors

import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.Json
import uk.gov.hmrc.domain.{Nino, SaUtr}
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{Accounts, PayeAccount, SaAccount}
import uk.gov.hmrc.play.http._
import uk.gov.hmrc.play.http.hooks.HttpHook
import uk.gov.hmrc.play.http.ws._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class DelegationConnectorSpec extends UnitSpec with WithFakeApplication with WireMockedSpec {

  private implicit val hc = HeaderCarrier()

  "The getDelegationData method" should {

    val delegationDataObject = DelegationData(
      principalName = "Dave Client",
      attorneyName = "Bob Agent",
      accounts = Accounts(
        paye = Some(PayeAccount(link = "http://paye/some/path", nino = Nino("AB123456D"))),
        sa = Some(SaAccount(link = "http://sa/some/utr", utr = SaUtr("1234567890")))
      ),
      link = Link(url = "http://taxplatform/some/dashboard", text = "Back to dashboard")
    )

    val delegationDataJson = Json.obj(
      "attorneyName" -> "Bob Agent",
      "principalName" -> "Dave Client",
      "link" -> Json.obj(
        "url" -> "http://taxplatform/some/dashboard",
        "text" -> "Back to dashboard"
      ),
      "accounts" -> Json.obj(
        "paye" -> Json.obj(
          "link" -> "http://paye/some/path",
          "nino" -> "AB123456D"
        ),
        "sa" -> Json.obj(
          "link" -> "http://sa/some/utr",
          "utr" -> "1234567890"
        )
      )
    ).toString()

    "return the delegation data returned from the service, if the response code is 200" in new TestCase {

      stubFor(get(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(200).withBody(delegationDataJson)))

      await(connector.getDelegationData(oid)) shouldBe Some(delegationDataObject)
    }

    "return None when the response code is 404" in new TestCase {

      stubFor(get(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(404)))

      await(connector.getDelegationData(oid)) shouldBe None
    }

    "throw an exception if the response code is anything other than 200 or 404" in new TestCase {

      val oid204 = "204oid"
      val oid400 = "400oid"
      val oid500 = "500oid"

      stubFor(get(urlEqualTo(s"/oid/$oid204")).willReturn(aResponse().withStatus(204)))
      stubFor(get(urlEqualTo(s"/oid/$oid400")).willReturn(aResponse().withStatus(400)))
      stubFor(get(urlEqualTo(s"/oid/$oid500")).willReturn(aResponse().withStatus(500)))

      a[DelegationServiceException] should be thrownBy await(connector.getDelegationData(oid204))
      a[DelegationServiceException] should be thrownBy await(connector.getDelegationData(oid400))
      a[DelegationServiceException] should be thrownBy await(connector.getDelegationData(oid500))
    }

    "throw an exception if the response is not valid JSON" in new TestCase {

      stubFor(get(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(200).withBody("{ not _ json :")))

      a[DelegationServiceException] should be thrownBy await(connector.getDelegationData(oid))
    }

    "throw an exception if the response is valid JSON, but not representing Delegation Data" in new TestCase {

      stubFor(get(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(200).withBody( """{"valid":"json"}""")))

      a[DelegationServiceException] should be thrownBy await(connector.getDelegationData(oid))
    }
  }

  "The startDelegation method" should {

    val delegationContextObject = DelegationContext(
      principalName = "Dave Client",
      attorneyName = "Bob Agent",
      principalTaxIdentifiers = TaxIdentifiers(
        paye = Some(Nino("AB123456D")),
        sa = Some(SaUtr("1234567890"))
      ),
      link = Link(url = "http://taxplatform/some/dashboard", text = "Back to dashboard")
    )

    val delegationContextJson = Json.obj(
      "attorneyName" -> "Bob Agent",
      "principalName" -> "Dave Client",
      "link" -> Json.obj(
        "url" -> "http://taxplatform/some/dashboard",
        "text" -> "Back to dashboard"
      ),
      "principalTaxIdentifiers" -> Json.obj(
        "paye" -> "AB123456D",
        "sa" -> "1234567890"
      )
    ).toString()

    "send the delegation data to the DelegationService, and succeed if the response code is 201" in new TestCase {

      stubFor(put(urlEqualTo(s"/oid/$oid")).withRequestBody(equalToJson(delegationContextJson)).willReturn(aResponse().withStatus(201)))

      await(connector.startDelegation(oid, delegationContextObject))

      verify(putRequestedFor(urlEqualTo(s"/oid/$oid")).withRequestBody(equalToJson(delegationContextJson)))
    }

    "send the delegation data to the DelegationService, and fail if the response code is anything other than 201" in new TestCase {

      val oid200 = "200oid"
      val oid204 = "204oid"
      val oid400 = "400oid"
      val oid500 = "500oid"

      stubFor(put(urlEqualTo(s"/oid/$oid200")).withRequestBody(equalToJson(delegationContextJson)).willReturn(aResponse().withStatus(200)))
      stubFor(put(urlEqualTo(s"/oid/$oid204")).withRequestBody(equalToJson(delegationContextJson)).willReturn(aResponse().withStatus(204)))
      stubFor(put(urlEqualTo(s"/oid/$oid400")).withRequestBody(equalToJson(delegationContextJson)).willReturn(aResponse().withStatus(400)))
      stubFor(put(urlEqualTo(s"/oid/$oid500")).withRequestBody(equalToJson(delegationContextJson)).willReturn(aResponse().withStatus(500)))

      a[DelegationServiceException] should be thrownBy await(connector.startDelegation(oid200, delegationContextObject))
      a[DelegationServiceException] should be thrownBy await(connector.startDelegation(oid204, delegationContextObject))
      a[BadRequestException] should be thrownBy await(connector.startDelegation(oid400, delegationContextObject))
      a[Upstream5xxResponse] should be thrownBy await(connector.startDelegation(oid500, delegationContextObject))
    }
  }

  "The endDelegation method" should {

    "request deletion from the Delegation Service and succeed if the result is 204" in new TestCase {

      stubFor(delete(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(204)))

      await(connector.endDelegation(oid))

      verify(deleteRequestedFor(urlEqualTo(s"/oid/$oid")))
    }

    "request deletion from the Delegation Service and succeed if the result is 404" in new TestCase {

      stubFor(delete(urlEqualTo(s"/oid/$oid")).willReturn(aResponse().withStatus(404)))

      await(connector.endDelegation(oid))

      verify(deleteRequestedFor(urlEqualTo(s"/oid/$oid")))
    }

    "request deletion from the Delegation Service and fail if the result anything other than 204 or 404" in new TestCase {

      val oid200 = "200oid"
      val oid201 = "201oid"
      val oid400 = "400oid"
      val oid500 = "500oid"

      stubFor(delete(urlEqualTo(s"/oid/$oid200")).willReturn(aResponse().withStatus(200)))
      stubFor(delete(urlEqualTo(s"/oid/$oid201")).willReturn(aResponse().withStatus(201)))
      stubFor(delete(urlEqualTo(s"/oid/$oid400")).willReturn(aResponse().withStatus(400)))
      stubFor(delete(urlEqualTo(s"/oid/$oid500")).willReturn(aResponse().withStatus(500)))

      a[DelegationServiceException] should be thrownBy await(connector.endDelegation(oid200))
      a[DelegationServiceException] should be thrownBy await(connector.endDelegation(oid201))
      a[DelegationServiceException] should be thrownBy await(connector.endDelegation(oid400))
      a[DelegationServiceException] should be thrownBy await(connector.endDelegation(oid500))
    }
  }

  trait TestCase extends MockitoSugar {

    val baseUrl = s"http://localhost:$Port"

    val connector = new DelegationConnector {

      override protected val serviceUrl = baseUrl

      override protected lazy val http = new WSHttp {
        override val hooks: Seq[HttpHook] = NoneRequired
      }
    }

    val oid = "1234"
  }



}
