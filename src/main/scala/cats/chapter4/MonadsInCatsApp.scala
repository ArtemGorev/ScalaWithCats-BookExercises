package cats.chapter4

import cats.Monad
import cats.instances.option._ // for Monad
import cats.instances.list._   // for Monad
import cats.instances.int._
import cats.Id

import cats.instances.future._ // for Monad
import scala.concurrent._
import scala.concurrent.duration._

//import cats.implicits._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._

object MonadsInCatsApp extends App {
  val opt1 = Monad[Option].pure(3)
  // opt1: Option[Int] = Some(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  // opt2: Option[Int] = Some(5)
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  // opt3: Option[Int] = Some(500)
  val list1 = Monad[List].pure(3)
  // list1: List[Int] = List(3)
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  // list2: List[Int] = List(1, 10, 2, 20, 3, 30)
  val list3 = Monad[List].map(list2)(a => a + 123)
  // list3: List[Int] = List(124, 133, 125, 143, 126, 153)

  println(opt1)
  println(opt2)
  println(opt3)
  println(list1)
  println(list2)
  println(list3)

  println("FUTURES & MONADS")

  import scala.concurrent.ExecutionContext.Implicits.global
  val fm     = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  println(Await.result(future, 1.second))

  println(1.pure[Option])
  println("helloWorld".pure[Option])
  println(1.32.pure[List])

//  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x * x + y * y))

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield (x * x + y * y)

  println(sumSquare(Option(1), Option(2)))
  println(sumSquare(List(1, 5), List(3, 2)))
  println(sumSquare(1: Id[Int], 3: Id[Int]))

  val a = Monad[Id].pure(3)
  // a: Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)

  val z = for {
    x <- a
    y <- b
  } yield x + y

  println(z)
}
