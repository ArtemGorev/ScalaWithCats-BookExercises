package cats.chapter11

import cats.instances.map._
import cats.instances.list._
import cats.kernel.CommutativeMonoid
import cats.syntax.all._
import cats.chapter11.GenericCounterApp._

object TraitCounterApp extends App {

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
  }
  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
      counter
  }

  implicit def counterInstance[K, V]: GCounter[Map, K, V] = {
    new GCounter[Map, K, V] {
      def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] =
        f + (k -> (f.getOrElse(k, m.empty) |+| v))

      def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        f1 |+| f2

      def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.toList.combineAll
    }
  }

  val g1      = Map("a" -> 7, "b" -> 3)
  val g2      = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]

  implicit val intMonoid = new CommutativeMonoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  val merged = counter.merge(g1, g2)
  val total  = counter.total(merged)

  println(counter)
  println(merged)
  println(total)
}
