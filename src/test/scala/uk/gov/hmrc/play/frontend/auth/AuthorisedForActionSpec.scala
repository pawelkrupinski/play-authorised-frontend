/*
 * Copyright 2015 HM Revenue & Customs
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

package uk.gov.hmrc.play.frontend.auth

import java.net.URI
import java.util.UUID

import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEachTestData, TestData}
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.domain.SaUtr
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{Accounts, Authority, ConfidenceLevel, SaAccount}
import uk.gov.hmrc.play.http.SessionKeys
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}
import uk.gov.hmrc.time.DateTimeUtils.now

import scala.concurrent.Future

class AuthorisedForActionSpec extends UnitSpec with BeforeAndAfterEachTestData with MockitoSugar with WithFakeApplication {

  val mockAuthConnector = mock[AuthConnector]

  val testedActions = new TestController with Actions {
    override protected implicit def authConnector: AuthConnector = mockAuthConnector
  }

  override protected def beforeEach(testData: TestData) {
    reset(mockAuthConnector)
  }

  "asserting authorization" should {
    "redirect to the login page when the userId is not found in the session " in  {
      val result = testedActions.testAuthorisation(FakeRequest())

      status(result) should be (303)
      redirectLocation(result).get shouldBe "/login"
    }

    "contain the result of the controller action in the response" in  {
      when(mockAuthConnector.currentAuthority(Matchers.any())).thenReturn(Some(saAuthority("jdensmore", "AB123456C")))
      val result = testedActions.testAuthorisation(requestFromLoggedInUser)

      status(result) should be (200)
      contentAsString(result) should include("jdensmore")
    }

    "redirect user to uplift if the user has insufficient confidence" in  {
      when(mockAuthConnector.currentAuthority(Matchers.any())).thenReturn(Some(lowAssuranceUser))
      val result = testedActions.testAuthorisation(requestFromLoggedInUser)

      status(result) should be (303)
      redirectLocation(result).get shouldBe "/mdtp/uplift?cl=500"
    }
  }

  def lowAssuranceUser: Authority = {
    Authority(s"/auth/oid/jdensmore", Accounts(sa = Some(SaAccount(s"/sa/individual/AB123456C", SaUtr("AB123456C")))), None, None, ConfidenceLevel.L100)
  }

  def requestFromLoggedInUser: FakeRequest[AnyContentAsEmpty.type] = {
    FakeRequest().withSession(
      SessionKeys.sessionId -> s"session-${UUID.randomUUID()}",
      SessionKeys.lastRequestTimestamp -> now.getMillis.toString,
      SessionKeys.userId -> "/auth/oid/jdensmore",
      SessionKeys.token -> "validtoken")
  }

  def saAuthority(id: String, utr: String): Authority =
    Authority(s"/auth/oid/$id",  Accounts(sa = Some(SaAccount(s"/sa/individual/$utr", SaUtr(utr)))), None, None, ConfidenceLevel.L500)
}

sealed class TestController
  extends Controller {

  this: Authoriser =>

  def testAuthorisation = AuthorisedFor(TestTaxRegime, pageVisibility = new UpliftingIdentityConfidencePredicate(ConfidenceLevel.L500, new URI("/mdtp/uplift?cl=500"))) {
    implicit authContext =>
      implicit request =>
        Ok("jdensmore")
  }

  def testAuthorisationWithRedirectCommand = AuthenticatedBy(authenticationProvider = TestAuthenticationProvider, redirectToOrigin = true, new NonNegotiableIdentityConfidencePredicate(ConfidenceLevel.L500)) {
    implicit authContext =>
      implicit request =>
        Ok("jdensmore")
  }

}

object TestAuthenticationProvider extends AuthenticationProvider {

  override val id = "TST"

  def login = "/login"

  def redirectToLogin(redirectToOrigin: Boolean)(implicit request: Request[AnyContent]) = Future.successful(Results.Redirect(login))

  def handleNotAuthenticated(redirectToOrigin: Boolean)(implicit request: Request[AnyContent]): PartialFunction[UserCredentials, Future[Either[AuthContext, FailureResult]]] = {
    case UserCredentials(None, None) =>
      redirectToLogin(redirectToOrigin).map(Right(_))
    case UserCredentials(Some(userId), None) =>
      redirectToLogin(redirectToOrigin).map(Right(_))
  }
}

object TestTaxRegime extends TaxRegime {

  def isAuthorised(accounts: Accounts) = accounts.sa.isDefined

  def authenticationType = TestAuthenticationProvider
}
