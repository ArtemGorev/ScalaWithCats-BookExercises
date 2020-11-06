package cats.chapter8

import java.util.Date

import cats.Monoid
import cats.instances.int._
import cats.instances.string._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.syntax.list._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MapReduceApp extends App {

  def foldMap[A, B : Monoid](seq: Vector[A])(f: A => B): B =
    seq.map(f).foldLeft(Monoid.empty[B])(Monoid.combine(_, _))

  val res1 = foldMap(Vector(1, 2, 3))(identity)
  val res2 = foldMap(Vector(1, 2, 3))(_.toString + "! ")
  val res3 = foldMap("Hello world!".toVector)(_.toString.toUpperCase)

  println(res1)
  println(res2)
  println(res3)

  println(Runtime.getRuntime.availableProcessors)

  def parallelFoldMap[A, B: Monoid](vec: Vector[A])(f: A => B): Future[B] = {
    vec
      .grouped(Runtime.getRuntime.availableProcessors)
      .map(x => Future { foldMap(x)(f) })
      .foldLeft(Monoid.empty[Future[B]])(Monoid.combine(_, _))
  }

  val d1 = (new Date).getTime
  private val i = 100000000
  val res4 = parallelFoldMap((1 to i).toVector)(identity)
  println(Await.result(res4, 1.second))
  val d2 = (new Date).getTime
  println(d2 - d1)

  val d3 = (new Date).getTime
  val res5 = foldMap((1 to i).toVector)(identity)
  println(res5)
  val d4 = (new Date).getTime
  println(d4 - d3)

}
