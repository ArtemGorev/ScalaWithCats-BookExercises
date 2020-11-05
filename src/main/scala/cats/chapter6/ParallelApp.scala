package cats.chapter6

import cats.implicits.catsKernelStdMonoidForVector
import cats.syntax.parallel._
import cats.instances.either._

object ParallelApp extends App {

  type ErrorOr[A] = Either[Vector[String], A]
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)

  val addTwo = (x: Int, y: Int) => x + y
  val addThree = (x: Int, y: Int, z: Int) => x + y + z

  val res2 = (error1, error2, success1).parMapN(addThree)
  val res3 = (success1, error2).parMapN(addTwo)

  println(res2)
  println(res3)


}
