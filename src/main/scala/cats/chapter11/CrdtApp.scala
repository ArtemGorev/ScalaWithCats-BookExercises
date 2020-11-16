package cats.chapter11

import cats.Monoid
import cats.kernel.CommutativeMonoid
import cats.syntax.monoid._
import cats.syntax.foldable._
import cats.instances.map._
import cats.instances.list._

object CrdtApp extends App {

  final case class GCounter[A](counters: Map[String, A]) {

    def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter[A] =
      GCounter(counters + (machine -> (counters.getOrElse(machine, monoid.empty) |+| amount)))

    def merge(that: GCounter[A])(implicit boundedSemiLattice: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(this.counters |+| that.counters)

    def total(implicit monoid: Monoid[A]): A = counters.values.toList.combineAll
  }

  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  implicit val intBoundedSemiLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1 max a2
    override def empty: Int = 0
  }

  implicit val setBoundedSemiLattice: BoundedSemiLattice[Set[_]] = new BoundedSemiLattice[Set[_]] {
    override def combine(a1: Set[_], a2: Set[_]): Set[_] = a1.union(a2)
    override def empty: Set[_] = Set.empty
  }


}
