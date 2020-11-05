package cats.chapter7

import cats.Monoid
import cats.Foldable
import cats.Eval
import cats.instances.vector._
import cats.instances.int._
import cats.syntax.foldable._
import cats.instances.list._
import cats.instances.string._
import cats.instances.lazyList._
import cats.instances.option._

object FoldableApp extends App {

  def show[A](list: List[A]): String = list.foldLeft("nil")((accum, item) => s"$item then $accum")
  println(show(Nil))
  println(show(List(1, 2, 3)))

  println(List("1", "2", "3").foldLeft("")((acc, head) => head + acc))
  println(List("1", "2", "3").foldRight("")((v, acc) => v + acc))

  println(List("1", "2", "3").foldLeft(List.empty[String])((tail, head) => head :: tail))
  println(List("1", "2", "3").foldRight(List.empty[String])((v, acc) => v :: acc))
  println(List("1", "2", "3").foldRight(List.empty[String])(_ :: _))

  val ints = List(1, 2, 3)
  val a    = Foldable[List].foldLeft(ints, 0)(_ + _)
//  val b    = Foldable[List].foldRight(ints, 0)(_ + _)

  val maybeInt = Option(123)
  val c        = Foldable[Option].foldLeft(maybeInt, "10")(_ + _)
  println(c)
//  println(a + b)

  def bigData = (1 to 10000).to(LazyList)
  println(bigData.foldRight(0L)(_ + _))
//   java.lang.StackOverflowError ...

  val eval: Eval[Long] = Foldable[LazyList]
    .foldRight(bigData, Eval.now(0L)) { (head, tail) => { tail.map(_ + head) } }

  println(eval.value)

  println(Foldable[Option].nonEmpty(Option(42)))
  println(Foldable[List].find(List(1, 2, 3, 4))(_ % 2 == 0))

  println(Foldable[List].combineAll(List("1", "2", "3")))
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))

  val intsss = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val d: Int = Foldable[List].compose(Foldable[Vector]).combineAll(intsss)
  println(s"d $d")

}
