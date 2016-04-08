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

package uk.gov.hmrc.play.frontend.auth

import uk.gov.hmrc.play.frontend.auth.connectors.domain.ConfidenceLevel
import uk.gov.hmrc.play.frontend.auth.connectors.domain.CredentialStrength
import uk.gov.hmrc.play.test.UnitSpec

class LoggedInUserSpec extends OidConversionSpec[LoggedInUser] with UnitSpec {

  "The oid method" should {
    successfullyConvertUserIdsToOids()
  }

  override protected def constructWithUserId(userId: String) = LoggedInUser(
    userId = userId,
    loggedInAt = None,
    previouslyLoggedInAt = None,
    governmentGatewayToken = None,
    credentialStrength = CredentialStrength.Strong,
    confidenceLevel = ConfidenceLevel.L500
  )

  override protected def oid(user: LoggedInUser): String = user.oid
}
