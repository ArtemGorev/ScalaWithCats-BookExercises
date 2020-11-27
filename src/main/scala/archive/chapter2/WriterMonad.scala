package archive.chapter2

import cats.data.Writer
import cats.implicits._
import cats.syntax._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


object WriterMonad extends App {
  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      }
      else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val z = 2.pure[Logged]
    .flatMap(c => Vector("JOPA").tell.map(_ => c)
  )

  println(z)

  //  println(factorial(5))
//  Await.result(Future.sequence(Vector(
//    Future(factorial(30)),
//    Future(factorial(20))
//  )).map(_.map(_.written).map(println)), 5.seconds)

  println(Vector(s"message").tell)
}
