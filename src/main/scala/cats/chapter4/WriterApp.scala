package cats.chapter4

import cats.data.Writer
import cats.syntax.writer._
import cats.syntax.applicative._
import cats.instances.vector._
import cats.instances.string._
import cats.instances.int._

object WriterApp extends App {

  type Logged[A] = Writer[Vector[String], A]

  val writer = Writer(Vector(
    "Record 1",
    "Record 2"
  ), 1000)

  println(writer)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield {
    println(a)
    println(b)
    a + b
  }
  println(writer1.run)

  type MyLogged[A] = Writer[String, A]
  val aa = 10.pure[MyLogged]
    .flatMap(x => x.writer("hello"))
    .flatMap(x => x.writer(" world"))
    .flatMap(x => (x + 32).writer(""))

  println(aa)
}
