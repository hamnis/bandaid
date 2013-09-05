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

import org.json4s.native
import org.specs2.mutable.Specification

class PatchSpec extends Specification {
  implicit object parser extends native.JsonMethods

  "Patch from RFC" should {
    "A.1.  Adding an Object Member" in {
      val json = parser.parse("""{ "foo": "bar"}""")
      val patch = Patch.parse("""[
                             |  { "op": "add", "path": "/baz", "value": "qux" }
                             |]""".stripMargin)

      val expected = parser.parse(
        """
          |{
          |  "baz": "qux",
          |  "foo": "bar"
          |}
        """.stripMargin)

      patch.apply(json) must be equalTo expected
    }
    "A.2.  Adding an Array Element" in {
      val json = parser.parse("""{ "foo": [ "bar", "baz" ] }""")

      val patch = Patch.parse("""[
                    |  { "op": "add", "path": "/foo/1", "value": "qux" }
                    |]""".stripMargin)

      val expected = parser.parse("""{ "foo": [ "bar", "qux", "baz" ] }""")

      patch.apply(json) must be equalTo expected
    }
    "A.3.  Removing an Object Member" in {
      val json = parser.parse("""{
                   |  "baz": "qux",
                   |  "foo": "bar"
                   |}""".stripMargin)
      val patch = Patch.parse(
        """[
          |  { "op": "remove", "path": "/baz" }
          |]""".stripMargin)

      val expected = parser.parse("""{ "foo": "bar" }""")

      patch.apply(json) must be equalTo expected
    }
    "A.4.  Removing an Array Element" in {
      val json = parser.parse("""{ "foo": [ "bar", "qux", "baz" ] }""")
      val patch = Patch.parse(
        """[
          |  { "op": "remove", "path": "/foo/1" }
          |]""".stripMargin)

      val expected = parser.parse("""{ "foo": [ "bar" , "baz"]}""")

      patch.apply(json) must be equalTo expected
    }
    "A.5.  Replacing a Value" in {
      val json = parser.parse("""{
                                |  "baz": "qux",
                                |  "foo": "bar"
                                |}""".stripMargin)

      val patch = Patch.parse(
        """[
          |  { "op": "replace", "path": "/baz", "value": "boo" }
          |]""".stripMargin)

      val expected = parser.parse("""{
                                    | "baz": "boo",
                                    | "foo": "bar"
                                    |}""".stripMargin)

      patch.apply(json) must be equalTo expected
    }
    "A.6.  Moving a Value" in {
      val json = parser.parse("""{
                                |  "foo": {
                                |  "bar": "baz",
                                |    "waldo": "fred"
                                |  },
                                |  "qux": {
                                |    "corge": "grault"
                                |  }
                                |}""".stripMargin)

      val patch = Patch.parse(
        """[
          |  { "op": "move", "from": "/foo/waldo", "path": "/qux/thud" }
          |]""".stripMargin)

      val expected = parser.parse("""{
                                    |  "foo": {
                                    |    "bar": "baz"
                                    |  },
                                    |  "qux": {
                                    |    "corge": "grault",
                                    |    "thud": "fred"
                                    |  }
                                    |}""".stripMargin)

      patch.apply(json) must be equalTo expected
    }
  }
}
