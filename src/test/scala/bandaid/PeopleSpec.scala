/*
 * Copyright 2013 Erlend Hamnaberg
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

class PeopleSpec extends Specification {
  implicit object parser extends native.JsonMethods

  "Example" should {
    "patch array item" in {
      val json = parser.parse(getClass.getResourceAsStream("/people.json"))
      val patch = Patch(List(Op.Replace("/people/0/age", JInt(33))))
      Pointer(patch(json)).select("/people/0/age") should beSome(JInt(33))
    }
  }
}
