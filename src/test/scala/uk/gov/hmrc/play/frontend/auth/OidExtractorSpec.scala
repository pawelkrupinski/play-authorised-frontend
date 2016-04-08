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

import uk.gov.hmrc.play.test.UnitSpec

class OidExtractorSpec extends OidConversionSpec[OidExtractorTestCase] with UnitSpec {

  override def constructWithUserId(userId: String) = OidExtractorTestCase (userId)
  override def oid(testCase: OidExtractorTestCase) = testCase.getOid

  "The userToOid function" should {
    successfullyConvertUserIdsToOids()
  }
}

case class OidExtractorTestCase(userId: String) {
  def getOid = OidExtractor.userIdToOid(userId)
}

trait OidConversionSpec[T] {

  self: UnitSpec =>

  protected def constructWithUserId(userId: String): T
  protected def oid(obj: T): String

  private def test(userId: String, expectedOid: String) = {
    val objectToTest = constructWithUserId(userId)
    val actualOid = oid(objectToTest)

    actualOid shouldBe expectedOid
  }

  def successfullyConvertUserIdsToOids() = {

    "return the value after the last slash" in {
      test("/auth/oid/1234567890", "1234567890")
      test("/abc/123/456", "456")
      test("/abcde", "abcde")
    }

    "just return the userId if there is no slash" in {
      test("abcd", "abcd")
    }
  }
}
