package cats.chapter4

import scala.util.Try
import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.instances.either._
import cats.instances.try_._

object MonadErrorApp extends App {
  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  val failure = monadError.raiseError("Errorishe")

  println(success)
  println(failure)

  val error = monadError.handleErrorWith(failure) {
    case "Badness" => monadError.pure("It's ok")
    case _         => monadError.raiseError("It's not ok")
  }

  println(error)

  println(monadError.ensure(success)("Number too low!")(_ > 1000))

  {
    val success = 42.pure[ErrorOr]
    val failure = "Badness".raiseError[ErrorOr, Int]

    val error = failure.handleErrorWith {
      case "Badness" => 256.pure[ErrorOr]
      case _         => "It's not ok".raiseError
    }

    println(s"error $error")

  }

  val exn: Throwable = new RuntimeException("It's all gone wrong")
  println(exn.raiseError[Try, Int])

  // exercise 4.5.4

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(age.pure[F])(new Exception("Age must be greater than or equal to 18"))(_ >= 18)

  println(validateAdult[Try](18))
  println(validateAdult[Try](8))
  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}
