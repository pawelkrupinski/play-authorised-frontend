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

import org.joda.time.DateTime
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{CredentialStrength, Accounts, Authority, ConfidenceLevel}

case class AuthContext(user: LoggedInUser, principal: Principal, attorney: Option[Attorney]) {
  lazy val isDelegating: Boolean = attorney.isDefined
}

object AuthContext {

  def apply(authority: Authority, governmentGatewayToken: Option[String] = None,
            nameFromSession: Option[String] = None,
            delegationData: Option[DelegationData] = None): AuthContext = {

    val (principalName: Option[String], accounts: Accounts, attorney: Option[Attorney]) = delegationData match {
      case Some(delegation) => (Some(delegation.principalName), delegation.accounts, Some(delegation.attorney))
      case None => (nameFromSession, authority.accounts, None)
    }

    AuthContext(
      user = LoggedInUser(
        userId = authority.uri,
        loggedInAt = authority.loggedInAt,
        previouslyLoggedInAt = authority.previouslyLoggedInAt,
        governmentGatewayToken = governmentGatewayToken,
        credentialStrength = authority.credentialStrength,
        confidenceLevel = authority.confidenceLevel
      ),
      principal = Principal(
        name = principalName,
        accounts = accounts
      ),
      attorney = attorney
    )
  }
}

case class LoggedInUser(userId: String,loggedInAt: Option[DateTime],
                        previouslyLoggedInAt: Option[DateTime],
                        governmentGatewayToken: Option[String],
                        credentialStrength: CredentialStrength,
                        confidenceLevel: ConfidenceLevel) {
  lazy val oid: String = OidExtractor.userIdToOid(userId)
}

case class Principal(name: Option[String], accounts: Accounts)

case class Attorney(name: String, returnLink: Link)

case class Link(url: String, text: String)

object OidExtractor {
  def userIdToOid(userId: String): String = userId.substring(userId.lastIndexOf("/") + 1)
}
