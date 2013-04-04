import org.specs2.mutable._
import org.json4s.native.JsonMethods._
import org.json4s._

class PointerSpec extends Specification {
  val rfcJson = parse(io.Source.fromInputStream(getClass.getResourceAsStream("/test.json")).mkString)
  
  "A RFC pointer" should {
    "select the whole document using empty string" in {
      Pointer.select("", rfcJson) must beEqualTo(rfcJson)
    }
    "select \"foo\" array property using /foo" in {
      Pointer.select("/foo", rfcJson) must beEqualTo(JArray(List(JString("bar"), JString("baz"))))
    }
    "select first item in \"foo\" array property using /foo/0" in {
      Pointer.select("/foo/0", rfcJson) must beEqualTo(JString("bar"))
    }
    "select \"\"    property using /" in {
      Pointer.select("/", rfcJson) must beEqualTo(JInt(0))
    }
    "select \"a/b\" property using /a~1b" in {
      Pointer.select("/a~1b", rfcJson) must beEqualTo(JInt(1))
    }    
    "select \"c%d\" property using /c%d" in {
      Pointer.select("/c%d", rfcJson) must beEqualTo(JInt(2))
    }
    "select \"e^f\" property using /e^f" in {
      Pointer.select("/e^f", rfcJson) must beEqualTo(JInt(3))
    }
    "select \"g|h\" property using /g|h" in {
      Pointer.select("/g|h", rfcJson) must beEqualTo(JInt(4))
    }
    "select \"i\\j\" property using /i\\j" in {
      Pointer.select("/i\\j", rfcJson) must beEqualTo(JInt(5))
    }
    "select \"k\"l\" property using /k\"l" in {
      Pointer.select("/k\"l", rfcJson) must beEqualTo(JInt(6))
    }
    "select \" \"   property using / " in {
      Pointer.select("/ ", rfcJson) must beEqualTo(JInt(7))
    }
    "select \"m~n\" property using /m~0n" in {
      Pointer.select("m~n", rfcJson) must beEqualTo(JInt(8))
    }
  }
}