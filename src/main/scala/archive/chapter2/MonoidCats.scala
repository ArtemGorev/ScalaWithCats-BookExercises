package archive.chapter2

import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}


case class Order(totalCost: Double, quantity: Double)

object MonoidCats extends App {
  println(Monoid[String].combine("Hi ", "there"))
  println(Monoid[String].empty)
  println(Semigroup[String].combine("Hi ", "there"))
  println(Monoid[Int].combine(32, 10))
  println(Monoid[Option[Int]].combine(Option(32), Option(10)))

  println("Hi " |+| "world" |+| Monoid[String].empty)
  println(20 |+| 12 |+| Monoid[Int].empty)

  def add[A: Monoid](items: List[A]): A = Monoid[A].combineAll(items)

  println(add(List(1, 2, 3, 4, 5)) === 15)
  println((add(List(Option(1), Option(14))) === Option(15)))
  println((add(List(Option(1), Option(14))) =!= Option(22)))

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  println(add(List(Order(1, 10), Order(2, 20))))

}
