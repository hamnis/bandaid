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

import org.json4s.JsonAST.{JNothing, JValue}
import scala.annotation.tailrec


case class Patch(ops: List[Op]) {
  def apply(json: JValue): JValue = {

    @tailrec def recur(ops: List[Op], json: JValue): JValue = {
      import Op._
      val p = Pointer(json)
      ops match {
        case Nil => json
        case x :: xs => x match {
          case Test(s, v) => recur(xs, p.select(s).filter(_ == v).getOrElse(JNothing))
          case Add(s, v) => recur(xs, p.add(s, v))
          case Remove(s) => recur(xs, p.remove(s))
          case Replace(s, v) => recur(xs, p.update(s, v))
          case Move(s, f) => recur(xs, p.select(f).map(v => Pointer(p.add(s, v)).remove(f)).getOrElse(json))
          case Copy(s, f) => recur(xs, p.select(f).map(v => p.add(s, v)).getOrElse(json))
        }
      }
    }
    recur(ops, json)
  }
}

object Patch {
  import org.json4s.JsonAST._
  import Op._
  def parse[A](input: org.json4s.JsonInput)(implicit methods: org.json4s.JsonMethods[A]): Patch = {
    val value = methods.parse(input, false)
    val list = value match {
      case JArray(a) => a.flatMap(v => v match {
        case j@JObject(_) => parseOp(Pointer(j)).toList
        case _ => sys.error("Not a valid Json patch document")

      })
      case _ => sys.error("Not a valid Json patch document")
    }
    Patch(list)
  }

  def parseOp[A](p: Pointer): Option[Op] = {
    for {
      JString(op) <- p.select("/op")
      JString(path) <- p.select("/path")
      value = p.select("/value")
      from = p.select("/from").map(_.values.toString)
    } yield {
      op match {
        case "test" => Test(path, value.getOrElse(sys.error("Not a valid Test op")))
        case "add" => Add(path, value.getOrElse(sys.error("Not a valid Add op")))
        case "remove" => Remove(path)
        case "replace" => Replace(path, value.getOrElse(sys.error("Not a valid Add op")))
        case "move" => Move(path, from.getOrElse(sys.error("Not a valid Move op")))
        case "copy" => Copy(path, from.getOrElse(sys.error("Not a valid Copy op")))
      }
    }
  }
}

sealed trait Op {
  def path: String
}

object Op {
  case class Test(path: String, value: JValue) extends Op

  case class Add(path: String, value: JValue) extends Op

  case class Remove(path: String) extends Op

  case class Replace(path: String, value: JValue) extends Op

  case class Move(path: String, from: String) extends Op

  case class Copy(path: String, from: String) extends Op
}

