package cats.chapter7

import cats.chapter7.TraverseApp.{listSequence, listTraverse}
import cats.data.Validated
import cats.instances.list._
import cats.instances.vector._
import cats.instances.option._ // for Applicative

object TraverseExerciseApp extends App {

  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  println(process(List(2, 4, 6))) // Some(List(2, 4, 6)
  println(process(List(1, 2, 3))) // None

  type ErrorsOr[A] = Validated[List[String], A]
  def processV(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  println(processV(List(2, 4, 6)))
  println(processV(List(1, 2, 3)))
}
