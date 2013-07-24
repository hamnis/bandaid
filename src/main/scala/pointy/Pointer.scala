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
  def select(selector: String): JValue
  def update(selector: String, update: JValue): JValue
}

/**
 Implementation of JSON Pointer. RFC-6901
 https://tools.ietf.org/html/rfc6901
*/
case class Pointer(document: JValue) extends JsonPointer {
  import Ref._
  
  def select(selector: String): JValue = select(toRefs(selector), document)

  def update(selector: String, replacement: JValue): JValue = {
    val refs = toRefs(selector)
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
    recur(refs, document)
  }

  def add(selector: String, replacement: JValue): JValue = {
    val refs = toRefs(selector)
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
    recur(refs, document)
  }

  def toRefs(selected: String): List[Ref] = {
    if (selected.trim.isEmpty) Nil
    else {
      val parts = ( if (selected.startsWith("/")) selected.substring(1) else selected).split("/").toList.map(unescape)
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
  
  private def select(parts: List[Ref], doc: JValue): JValue = {
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
    recur(parts, doc)
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
