package archive.chapter2

object AllSetForMonoids extends App {
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  }

  val intSetMonoid = Monoid[Set[Int]];
  val value: Set[Int] = intSetMonoid.combine(Set(1), Set(2, 3))
  println(value);

}
