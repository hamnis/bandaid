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

package pointy

import org.json4s.JsonAST._
import annotation.tailrec

trait JsonPointer {
  def document: JValue
  def select(selector: Selector): JValue
  def update(selector: Selector, update: JValue): JValue
  def add(selector: Selector, newValue: JValue): JValue
}

/**
 Implementation of JSON Pointer. RFC-6901
 https://tools.ietf.org/html/rfc6901
*/
case class Pointer(document: JValue) extends JsonPointer {
  import Selector._

  def select(selector: Selector): JValue = {
    @tailrec def recur(parts: List[Ref], in: JValue): JValue = {
      parts match {
        case Nil => in
        case ArrayRef(x) :: xs => in match {
          case JArray(list) => if (list.isDefinedAt(x)) recur(xs, list(x)) else sys.error("List index '%s' is out-of-bounds".format(x))
          case _ => JNothing
        }
        case EndOfArray :: _ => sys.error("List index is out-of-bounds")
        case PropertyRef(name) :: xs => in match {
          case JObject(list) => recur(xs, list.find{case JField(n, v) => n == name}.map(_._2).getOrElse(sys.error("Field not found")))
          case _ => JNothing
        }
      }
    }
    recur(selector.refs, document)
  }

  def update(selector: Selector, replacement: JValue): JValue = {
    def recur(s: List[Ref], in: JValue): JValue = {
      s match {
        case Nil => in
        case PropertyRef(name) :: xs => in match {
          case JObject(fields) => JObject(
             fields map {
               case JField(`name`, v) => JField(name, if (xs == Nil) replacement else recur(xs , v))
               case field => field
             }
          )
          case other => other
        }
        case ArrayRef(i) :: xs => in match {
          case a@JArray(arr) => if (xs == Nil) {
            if (arr.isDefinedAt(i)) JArray(arr.updated(i, replacement)) else sys.error("List index '%s' is out-of-bounds".format(i))
          } else recur(xs, a)
          case other => other
        }
        case EndOfArray :: xs => sys.error("List index is out-of-bounds")
      }
    }
    recur(selector.refs, document)
  }

  def add(selector: Selector, replacement: JValue): JValue = {
    def recur(s: List[Ref], in: JValue): JValue = {
      s match {
        case Nil => in
        case PropertyRef(name) :: xs => in match {
          case JObject(fields) => {
            val updated = fields map {
              case JField(`name`, v) => JField(name, if (xs == Nil) sys.error("Property with name %s already exists".format(name)) else recur(xs , v))
              case field => field
            }
            JObject(
              updated :+ JField(name, replacement)
            )
          }
          case other => other
        }
        case ArrayRef(i) :: xs => in match {
          case a@JArray(arr) => if (xs == Nil) JArray(arr.patch(i + 1, List(replacement), 0)) else recur(xs, a)
          case other => other
        }
        case EndOfArray :: xs => in match {
          case a@JArray(arr) => if (xs == Nil) JArray(arr :+ replacement) else recur(xs, a)
          case other => other
        }
      }
    }
    recur(selector.refs, document)
  }
}

case class Path(seg: List[String]) {
  def toSelector = seg match {
    case Nil => Selector("")
    case xs => Selector(xs.mkString("/", "/", ""))
  }
}

case class Selector(in: String) {

  private[pointy] val refs: List[Selector.Ref] = {
    import Selector._
    if (in.trim.isEmpty) Nil
    else {
      val parts = ( if (in.startsWith("/")) in.substring(1) else in).split("/").toList.map(unescape)
      @tailrec def recur(parts: List[String], acc:List[Ref]): List[Ref] = {
        parts match {
          case Nil => acc
          case AsInt(x) :: xs => recur(xs, acc ++ List(ArrayRef(x)))
          case x :: xs => recur(xs, acc ++ List(PropertyRef(x)))
        }
      }
      recur(parts, Nil)
    }
  }
  private def unescape(str: String): String = str.replace("~1", "/").replace("~0", "~")
}

object Selector {
  implicit def toSelector(s: String) = Selector(s)

  private[pointy] sealed trait Ref

  private[pointy] case class ArrayRef(index:Int) extends Ref
  private[pointy] case object EndOfArray extends Ref
  private[pointy] case class PropertyRef(name:String) extends Ref


  private[pointy] object AsInt {
    import util.control.Exception.allCatch
    def unapply(str: String): Option[Int] = allCatch.opt(str.toInt)
  }
}
