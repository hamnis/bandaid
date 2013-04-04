import org.json4s.JsonAST._
import annotation.tailrec

/**
 Implementation of JSON Pointer. RFC-6901
 https://tools.ietf.org/html/rfc6901
*/
object Pointer {
  import Ref._
  
  def select(selector: String, doc: JValue): JValue = select(toRefs(selector), doc)
  
  def toRefs(selected: String): List[Ref] = {
    if (selected.trim.isEmpty) Nil
    else {
      val parts = ( if (selected.startsWith("/")) selected.substring(1) else selected).split("/").toList.map(unescape)
      @tailrec def req(parts: List[String], acc:List[Ref]): List[Ref] = {
        parts match {
          case Nil => acc
          case AsInt(x) :: xs => req(xs, acc ++ List(ArrayRef(x)))
          case x :: xs => req(xs, acc ++ List(PropertyRef(x)))
        }      
      }
      req(parts, Nil)
    }
  }
  
  private def select(parts: List[Ref], doc: JValue): JValue = {
    @tailrec def req(parts: List[Ref], in: JValue): JValue = {
      parts match {
        case Nil => in
        case ArrayRef(x) :: xs => in match {
          case JArray(list) if (list.isDefinedAt(x)) => req(xs, list(x))
          case _ => JNothing
        }
        case EndOfArray :: _ => JNothing
        case PropertyRef(name) :: xs => in match {
          case JObject(list) => req(xs, list.find{case JField(n, v) => n == name}.map(_._2).getOrElse(JNothing))
          case _ => JNothing
        }
      }
    }
    req(parts, doc)
  }
  
  private def unescape(str: String): String = str.replace("~1", "/").replace("~0", "~")
}

object AsInt {
  import util.control.Exception.allCatch
  def unapply(str: String): Option[Int] = allCatch.opt(str.toInt)
}

sealed trait Ref

object Ref {
  case class ArrayRef(index:Int) extends Ref
  case object EndOfArray extends Ref
  case class PropertyRef(name:String) extends Ref
}
