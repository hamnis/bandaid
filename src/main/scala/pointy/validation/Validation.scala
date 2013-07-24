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

package pointy.validation

trait Semigroup[A] {
  // associative
  def append(a1: A, a2: A): A
}

object Semigroup {
  implicit def ListSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(a1: List[A], a2: List[A]) = a1 ::: a2
  }
}

sealed trait Validation[E, X] {
  def map[Y](f: X => Y): Validation[E, Y]
    = this match {
    case Failure(e) => Failure(e)
    case Success(x) => Success(f(x))
  }

  // applicative functor
  def <<*>>[Y](f: Validation[E, X => Y])
            (implicit s: Semigroup[E]): Validation[E, Y]
    = (this, f) match {
      case (Failure(e1), Failure(e2)) => Failure(s append (e1, e2))
      case (Failure(e1), Success(_))  => Failure(e1)
      case (Success(_), Failure(e2))  => Failure(e2)
      case (Success(x), Success(k))   => Success(k(x))
    }
}

final case class Failure[E, X](e: E) extends Validation[E, X]
final case class Success[E, X](x: X) extends Validation[E, X]