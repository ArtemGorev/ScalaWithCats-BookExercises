package cats.chapter4

import cats.implicits._

import scala.concurrent.Future

object MonadIntroApp extends App {
  def parseInt(str: String): Option[Int]  = scala.util.Try(str.toInt).toOption
  def divide(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

  parseInt("100").flatMap(divide(_, 5)).flatMap(a => println(s"$a").pure[Option])

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      ans  <- divide(aNum, bNum)
    } yield ans

  println(stringDivideBy("6", "2"))
  // res0: Option[Int] = Some(3)
  println(stringDivideBy("6", "0"))
  // res1: Option[Int] = None
  println(stringDivideBy("6", "foo"))

  println((1 to 3).toList.flatMap(x => (4 to 5).toList.map(y => (x, y))))

  val z = for {
    x <- (1 to 3).toList
    y <- (4 to 5).toList
  } yield (x, y)

  println(z)

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(a => pure(func(a)))
  }

  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def pure[A](a: A): Id[A]                                 = a
    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
  }

}
