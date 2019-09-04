/*
 * Copyright 2019 Erlend Hamnaberg
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bandaid

import org.json4s._
import org.json4s.native
import org.specs2.mutable.Specification
import Selector._
import Ref._

class PeopleSpec extends Specification {
  implicit object parser extends native.JsonMethods

  "Example" should {
    "patch array item" in {
      val json = parser.parse(getClass.getResourceAsStream("/people.json"))
      val path = StringSelector("/people/0/age")
      val path2: Path = PropertyRef("people") :: ArrayRef(0) :: PropertyRef("age") :: Nil
      val patch = Patch(List(Op.Replace(path, JInt(33))))
      Pointer(patch(json)).select(path) should beSome(JInt(33))

      val patch2 = Patch(List(Op.Replace(path2, JInt(33))))
      Pointer(patch2(json)).select(path2) should beSome(JInt(33))
    }

    "patch object somewhere in array item" in {
      val json = parser.parse(getClass.getResourceAsStream("/people.json"))
      val patch = Patch(List(Op.Add(StringSelector("/people/0/links/something"), JString("http://example.com"))))
      Pointer(patch(json)).select("/people/0/links/something") should beSome(JString("http://example.com"))
    }
  }
}
