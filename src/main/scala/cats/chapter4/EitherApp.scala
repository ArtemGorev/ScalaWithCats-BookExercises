package cats.chapter4

import cats.syntax.either._

import scala.util.Try

object EitherApp extends App {

  val either1: Either[String, Int] = Right(10)
  val either2: Either[String, Int] = Right(32)
  val r = for {
    a <- either1
    b <- either2
  } yield a + b
  println(r)

  val a = 3.asRight[String]
  val b = 39.asRight[String]
  val c = for {
    x <- a
    y <- b
  } yield x * x + y * y
  println(c)

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      num match {
        case num if num > 0 => accumulator.map(_ + 1)
        case _              => Left("Negative. Stopping!")
      }
    }
  println(countPositive(List(1, 2, 3, -1)))
  println(countPositive(List(1, 2, 3)))

  println("Error".asLeft[Int].getOrElse(0))
  println("Error".asLeft[Int].orElse(2.asRight[String]))
  println((-1).asRight[String].ensure("Must be non-negative")(_ > 0))
  println(Either.fromTry(Try("foo".toInt)))

  val left  = "error".asLeft[Int]
  val left2 = 10.asRight[String]

  val asLeft = left2.recover {
    case _: String => -1
  }
  println(asLeft)

  val asLeft2 = left2.recoverWith {
    case _: String => (-1).asRight[String]
  }
  println(asLeft2)

  val res = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100
  println(res)

  // ERROR HANDLING
  println("========ERROR HANDLING========")

  sealed trait LoginError                              extends Product with Serializable
  final case class UserNotFound(username: String)      extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError                          extends LoginError
  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")

      case _ => println("Error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  // result1: LoginResult = Right(User("dave", "passw0rd"))
  val result2: LoginResult = UserNotFound("dave").asLeft
  // result2: LoginResult = Left(UserNotFound("dave"))

  result1.fold(handleError, println)
  // User(dave, passw0rd)
  result2.fold(handleError, println)
  // User not found: dave

}
