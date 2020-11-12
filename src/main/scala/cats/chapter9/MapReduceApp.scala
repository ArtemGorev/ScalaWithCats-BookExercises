package cats.chapter9

import java.util.Date

import cats.Monoid
import cats.instances.int._
import cats.instances.string._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object MapReduceApp extends App {

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.map(f).foldLeft(Monoid.empty[B])(Monoid.combine(_, _))

  val res1 = foldMap(Vector(1, 2, 3))(identity)
  val res2 = foldMap(Vector(1, 2, 3))(_.toString + "! ")
  val res3 = foldMap("Hello world!".toVector)(_.toString.toUpperCase)

  println(res1)
  println(res2)
  println(res3)

  println(Runtime.getRuntime.availableProcessors)

  def parallelFoldMap[A, B: Monoid](vec: Vector[A])(func: A => B): Future[B] =
    Future
      .sequence(
        vec
          .grouped((1.0 * vec.size / Runtime.getRuntime.availableProcessors).ceil.toInt)
          .map(group =>
            Future(
              foldMap(group)(func)
            )
          )
      )
      .map(_.foldLeft(Monoid.empty[B])(Monoid.combine(_, _)))

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups: Iterator[Vector[A]] = values.grouped(groupSize)

    val futures: Iterator[Future[B]] = groups.map(group => Future(foldMap(group)(func)))
    Future.sequence(futures).map { iterable => iterable.foldLeft(Monoid[B].empty)(_ |+| _) }
  }

  val d1 = (new Date).getTime
  val i = 1000000
  val res4 = parallelFoldMap((1 to i).toVector)(identity)
  println(Await.result(res4, 1.second))
  val d2 = (new Date).getTime
  println(d2 - d1)

  val d3 = (new Date).getTime
  val res5 = foldMap((1 to i).toVector)(identity)
  println(res5)
  val d4 = (new Date).getTime
  println(d4 - d3)
  val res6 = parallelFoldMap2((1 to i).toVector)(identity)
  println(Await.result(res6, 1.second))
  val d5 = (new Date).getTime
  println(d5 - d4)

}
