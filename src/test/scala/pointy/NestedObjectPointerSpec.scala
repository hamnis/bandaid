import org.specs2.mutable._
import org.json4s.native.JsonMethods._
import org.json4s._

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
    "select the whole document" in {
      Pointer.select("", lotteryJson) must beEqualTo(lotteryJson)
    }
    "select \"lotto\" object property" in {
      Pointer.select("/lotto", lotteryJson) must beEqualTo(lotteryJson \ "lotto")
    }
    "select \"lotto-id\" property in nested lotto object" in {
      Pointer.select("/lotto/lotto-id", lotteryJson) must beEqualTo(JInt(5))
    }
    "select second number in \"winning-numbers\" property in nested lotto object" in {
      Pointer.select("/lotto/winning-numbers/1", lotteryJson) must beEqualTo(JInt(45))
    }
    "select first winner in \"winners\" property in nested lotto object" in {
      Pointer.select("/lotto/winners/0", lotteryJson) must beEqualTo(parse("""{
            "winner-id":23,
            "numbers":[2,45,34,23,3,5]
          }"""))
    }
    "select none existing index 4 in \"winners\" property in nested lotto object" in {
      Pointer.select("/lotto/winners/4", lotteryJson) must beEqualTo(JNothing)
    }
  }
}