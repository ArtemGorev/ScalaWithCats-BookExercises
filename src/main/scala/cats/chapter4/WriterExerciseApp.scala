package cats.chapter4

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object WriterExerciseApp extends App {

  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial1(n: Int): Logged[Int] = {
    if (n < 1) {
      1.pure[Logged]
    } else {
      slowly(factorial1(n - 1).flatMap(x => (x * n).writer(Vector(s"Step $n"))))
    }
  }

  def factorial2(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial2(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  println(
    Await.result(Future.sequence(Vector(Future(factorial1(3)), Future(factorial2(2)))), 50.seconds)
  )
}
