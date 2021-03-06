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

import org.specs2.mutable._
import org.json4s._
import org.json4s.native.JsonMethods._

class NestedObjectPointerSpec extends Specification {
  lazy val lotteryJson = parse("""
      {
        "lotto":{
          "lotto-id":5,
          "winning-numbers":[2,45,34,23,7,5,3],
          "winners":[{
            "winner-id":23,
            "numbers":[2,45,34,23,3,5]
          },{
            "winner-id":54,
            "numbers":[52,3,12,11,18,22]
          }]
        }
      }""")
  
  "A pointer" should {
    val pointer = Pointer(lotteryJson)
    "select the whole document" in {
      pointer.select("") must beEqualTo(Some(lotteryJson))
    }
    "select \"lotto\" object property" in {
      pointer.select("/lotto") must beEqualTo(Some(lotteryJson \ "lotto"))
    }
    "select \"lotto-id\" property in nested lotto object" in {
      pointer.select("/lotto/lotto-id") must beEqualTo(Some(JInt(5)))
    }
    "select second number in \"winning-numbers\" property in nested lotto object" in {
      pointer.select("/lotto/winning-numbers/1") must beEqualTo(Some(JInt(45)))
    }
    "select first winner in \"winners\" property in nested lotto object" in {
      pointer.select("/lotto/winners/0") must beEqualTo(Some(parse("""{
            "winner-id":23,
            "numbers":[2,45,34,23,3,5]
          }""")))
    }
    "select none existing index 4 in \"winners\" property in nested lotto object" in {
      pointer.select("/lotto/winners/4") must beNone
    }
    "update first winner in \"winners\" property in nested lotto object" in {
      val expected = parse("""{
            "winner-id": 300,
            "numbers":[10,20,40,60,3,5,23]
          }""")
      val update = pointer.update("/lotto/winners/0", expected)
      Pointer(update).select("/lotto/winners/0") must beEqualTo(Some(expected))
    }
  }
}