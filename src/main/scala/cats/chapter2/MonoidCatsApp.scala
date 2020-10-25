package cats.chapter2

import cats._
import cats.implicits._

object MonoidCatsApp extends App {

  println(Monoid[String].combine("Hi", "there"))
  println(Monoid[String].empty)

  println(Monoid[Int].combine(1, 2))
  println(Monoid[Int].empty)

  val a = Option("World")
  val b = Option("Hello")
  val c = None

  println(Monoid[Option[String]].combine(a, b))
  println(Monoid[Option[String]].combine(a, c))

//  implicit val optionStringMonoid = Monoid[Option[String]]

  object OptionObjectOps {
    def combine(a: Option[String], b: Option[String])(implicit monoid: Monoid[Option[String]]) =
      monoid.combine(a, b)
  }

  println(OptionObjectOps.combine(a, b))

  println("|+|-|+|-|+|-|+|-|+|")
  println("Hello" |+| "_" |+| "World")
  println(1 |+| 2 |+| +Monoid[Int].empty)
  println(a |+| b)
  println(a |+| c |+| b)

  println("SuperAdder v3.5a-32")
//  def add(items: List[Int]): Int = items.fold(0)(Monoid[Int].combine)
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A = items.fold(monoid.empty)(monoid.combine)

  val x = Option(1)
  val y = Option(2)
  val z = None

  println(add(List(1, 2, 3, 4, 5)))
  println(add(List(x, y, z)))
  println(add(List("x", "y", "z")))

  case class Order(totalCost: Double, quantity: Double)

  val order1 = Order(100, 10)
  val order2 = Order(50, 20)
  val order3 = Order(150, 15)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  println(add(List(order1, order2, order3)))
  println(add(List(Option(order1), Option(order2), Option(order3))))

}
