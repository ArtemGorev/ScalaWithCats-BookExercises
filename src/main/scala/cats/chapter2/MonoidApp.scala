package cats.chapter2

object MonoidApp extends App {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  implicit val and: Monoid[Boolean] = new Monoid[Boolean] {
    override val empty: Boolean                           = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val or: Monoid[Boolean] = new Monoid[Boolean] {
    override val empty: Boolean                           = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val xor: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean                           = false
    override def combine(x: Boolean, y: Boolean): Boolean = x != y
  }

  object BooleanOps {
    def combine[A](x: A, y: A)(implicit m: Monoid[A]): A = m.combine(x, y)
    def empty[A](implicit m: Monoid[A]): A               = m.empty
  }

  val x = true
  val y = true
  val z = false

  // checking xor
  println(
    BooleanOps.combine(x, BooleanOps.combine(y, z)(xor))(xor) ==
      BooleanOps.combine(BooleanOps.combine(x, y)(xor), z)(xor)
  )

  println(
    (BooleanOps.combine(x, BooleanOps.empty(xor))(xor) == x) &&
      (BooleanOps.combine(BooleanOps.empty(xor), x)(xor) == x)
  )

}
