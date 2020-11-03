package cats.chapter5

import cats.data.{EitherT, OptionT}
import cats.instances.list._
import cats.instances.option._
import cats.instances.future._
import cats.syntax.applicative._
import cats.instances.either._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Future

case class User(name: String)

object MonadTransformerApp extends App {
  println("MonadTransformerApp")

  def lookupUser(id: Long): Either[Error, Option[User]] = Right(Some(User("String")))

  def lookupUserName(id: Long): Either[Error, Option[String]] =
    for {
      optUser <- lookupUser(id)
    } yield {
      for { user <- optUser } yield user.name
    }

  type ListOption[A] = OptionT[List, A]

  val result1 = OptionT(List(Option(10)))
  val result2 = 32.pure[ListOption] // List(Some(10))

  println(s"result1 $result1")
  println(s"result2 $result2")

  val res3 = for {
    a <- result1
    b <- result2
  } yield a + b

  println(s"res3 $res3")

  type OptionOption[A] = OptionT[Option, A]

  val result11 = OptionT(Option(Option(10)))
  val result21 = 32.pure[OptionOption] // List(Some(10))

  println(s"result1 $result11")
  println(s"result2 $result21")

  val res31 = for {
    a <- result11
    b <- result21
  } yield a + b

  println(s"res3 $res31")

  type ErrorOr[A]       = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))
  println(c.getOrElse(0).getOrElse(0))
  println(s"c $c")

  type FutureEither[A]       = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  println(futureEitherOr)
  println(Await.result(futureEitherOr.value.value, 1.second))
}
