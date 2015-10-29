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

import play.api.mvc.Results._
import play.api.mvc.{Result, _}
import uk.gov.hmrc.play.frontend.auth.connectors.domain.ConfidenceLevel
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent._

trait PageVisibilityPredicate {
  def isVisible(authContext: AuthContext, request: Request[AnyContent]): Future[Boolean]

  def nonVisibleResult: Result = NotFound
}

class UpliftingIdentityConfidencePredicate(requiredConfidenceLevel: ConfidenceLevel, upliftConfidenceUri: URI)
  extends PageVisibilityPredicate {
  override def isVisible(authContext: AuthContext, request: Request[AnyContent]): Future[Boolean] =
    Future.successful(authContext.user.confidenceLevel >= requiredConfidenceLevel)

  override def nonVisibleResult: Result = Redirect(upliftConfidenceUri.toString)
}

class NonNegotiableIdentityConfidencePredicate(requiredConfidenceLevel: ConfidenceLevel)
  extends PageVisibilityPredicate {
  override def isVisible(authContext: AuthContext, request: Request[AnyContent]): Future[Boolean] =
    Future.successful(authContext.user.confidenceLevel >= requiredConfidenceLevel)

  override def nonVisibleResult: Result = Forbidden
}

private[auth] object WithPageVisibility {

  def apply(predicate: PageVisibilityPredicate, authContext: AuthContext)(action: AuthContext => Action[AnyContent]): Action[AnyContent] =
    Action.async {
      request =>
        implicit val hc = HeaderCarrier.fromHeadersAndSession(request.headers,Some(request.session) )
        predicate.isVisible(authContext, request).flatMap { visible =>
          if (visible)
            action(authContext)(request)
          else
            Action(predicate.nonVisibleResult)(request)
        }
    }
}

object AllowAll extends PageVisibilityPredicate {
  def isVisible(authContext: AuthContext, request: Request[AnyContent]) = Future.successful(true)
}
