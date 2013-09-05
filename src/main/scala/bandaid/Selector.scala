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

import scala.annotation.tailrec

trait Selector[A] {
  def apply(value: A): List[Ref]
}

object Selector {
  implicit object StringSelector extends Selector[String] {
    def apply(value: String): List[Ref] = {
      import Ref._
      if (value.trim.isEmpty) Nil
      else {
        val parts = ( if (value.startsWith("/")) value.substring(1) else value).split("/").toList.map(unescape)
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

    implicit object PathSelector extends Selector[List[String]] {
      def apply(value: List[String]) = value match {
        case Nil => StringSelector("")
        case xs => StringSelector(xs.mkString("/", "/", ""))
      }
    }

    def unescape(str: String): String = str.replace("~1", "/").replace("~0", "~")
  }
}

sealed trait Ref

object Ref{
  case class ArrayRef(index:Int) extends Ref
  case object EndOfArray extends Ref
  case class PropertyRef(name:String) extends Ref

  private[bandaid] object AsInt {
    import util.control.Exception.allCatch
    def unapply(str: String): Option[Int] = allCatch.opt(str.toInt)
  }
}
