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

package uk.gov.hmrc.play.frontend.auth.connectors.domain

import play.api.libs.json.Json
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

class AuthoritySpec extends UnitSpec with WithFakeApplication {

  "Authority" should {

    "round trip correctly to and from JSON" in {

      val authority = Authority("/auth/some-oid",
        Accounts(paye = Some(PayeAccount("/paye/AA000002B", Nino("AA000002B"))), iht = Some(IhtAccount("/iht/AA000002B", Nino("AA000002B")))),
        None,
        None,
        ConfidenceLevel.L500)

      val jsonString = Json.toJson(authority).toString()

      jsonString should include ("\"paye\":{\"link\":\"/paye/AA000002B\",\"nino\":\"AA000002B\"}")
      jsonString should include ("\"iht\":{\"link\":\"/iht/AA000002B\",\"nino\":\"AA000002B\"}")

      val roundTrippedAuthority = Json.parse(jsonString).as[Authority]

      roundTrippedAuthority shouldBe authority
    }

  }

}
