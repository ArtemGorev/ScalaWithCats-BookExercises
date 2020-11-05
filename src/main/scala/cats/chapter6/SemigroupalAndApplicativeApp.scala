package cats.chapter6

import cats.Semigroupal
import cats.implicits.{catsStdInstancesForFuture, catsStdInstancesForList}
import cats.instances.option._
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object SemigroupalAndApplicativeApp extends App {

  val res1 = Semigroupal[Option].product(Some(123), Some("abc"))
  println(res1)

  val res2 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  println(res2)

  val res3 = Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
  println(res3)

  val res4 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  println(res4)

  val res5 = (Option(123), Option("abc")).tupled
  println(res5)

  final case class Cat(name: String, born: Int, color: String)
  val res6 = (Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply)
  println(res6)

  val a = Future("Future 1")
  val b = Future("Future 2")

  val c: Future[(String, String)] = for {
    x <- a
    y <- b
  } yield (x, y)

  val d: Future[(String, String)] = (a, b).tupled

  val res7 = Semigroupal[List].product(List(1, 2), List(3, 4))
  println(res7)
}
