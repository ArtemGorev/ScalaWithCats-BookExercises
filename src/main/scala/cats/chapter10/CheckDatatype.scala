package cats.chapter10

import cats.Semigroup
import cats.syntax.apply._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

object CheckDatatype extends App {

  sealed trait Check[E, A] {

    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          (left(a), right(a)) match {
            case (Valid(_), Valid(_)) => Valid(a)
            case (Invalid(_), Valid(_)) => Valid(a)
            case (Valid(_), Invalid(_)) => Valid(a)
            case (Invalid(v1), Invalid(v2)) => Invalid(s.combine(v1, v2))
          }
      }

//    def map[B](check: Check[E, A])(func: A => B)(implicit s: Semigroup[E]): Check[E, B] =
//      (a: A) =>
//        check(a) match {
//          case Valid(value: A) => Valid(func(value))
//          case Invalid(e: E)   => Invalid(e)
//        }
  }

  object Check {

    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

    final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  }

  val more10 = Check.Pure[List[String], Int](a => if (a > 10) Valid(a) else Invalid(List("less then 10")))
  val positive = Check.Pure[List[String], Int](a => if (a > 0) Valid(a) else Invalid(List("not positive")))

//  println(more10(5))
//  println(more10(15))
//
//  println(Check.And(more10, positive)(11))
//  println(Check.And(more10, positive)(0))

}
