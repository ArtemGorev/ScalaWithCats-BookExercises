package cats.chapter2

import scala.collection.immutable.Set

object MonoidSetApp extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  implicit def setUnion[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set[A]()

    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }

  println(setUnion.combine(Set[Int](1, 2, 3), setUnion.empty))
  println(setUnion.combine(setUnion.empty, Set[Int](1, 2, 3)))

  println(setUnion.combine(Set[Int](4, 5, 6), Set[Int](1, 2, 3)))
  println(setUnion.combine(Set[Int](2, 3, 1), Set[Int](1, 2, 3)))
  println(setUnion.combine(Set[Int](2, 3, 4), Set[Int](1, 2, 3)))

  println(setUnion.combine(Set[String]("1", "2", "3"), setUnion.empty))
  println(setUnion.combine(setUnion.empty, setUnion.empty))
  println(setUnion.combine(setUnion.empty, Set[String]("1", "2", "3")))

  println(setUnion.combine(Set[String]("4", "5", "6"), Set[String]("1", "2", "3")))
  println(setUnion.combine(Set[String]("2", "3", "1"), Set[String]("1", "2", "3")))
  println(setUnion.combine(Set[String]("2", "3", "4"), Set[String]("1", "2", "3")))

  implicit def setIntersection[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty[A]

    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }
}
