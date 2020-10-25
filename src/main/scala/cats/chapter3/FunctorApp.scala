package cats.chapter3

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

import cats.instances.function._ // for Functor
import cats.syntax.functor._

object FunctorApp extends App {

  val x: Option[Int] = Option(1)
  println(x.map(i => s"$i + 3"))
  val y: Option[Int] = None
  println(y.map(_ + 1))

  val future1 = {
    // Initialize Random with a fixed seed:
    val r = new Random(0L)

    // nextInt has the side-effect of moving to
    // the next random number in the sequence:
    val x = Future(r.nextInt())

    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  val future2 = {
    val r = new Random(0L)

    for {
      a <- Future(r.nextInt())
      b <- Future(r.nextInt())
    } yield (a, b)
  }

  val result1 = Await.result(future1, 1.second)
  // result1: (Int, Int) = (-1155484576, -1155484576)
  val result2 = Await.result(future2, 1.second)
  // result2: (Int, Int) = (-1155484576, -723955400)”
  println(result1)
  println(result2)


  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => s"${x}!")

  println(func(123))

}
