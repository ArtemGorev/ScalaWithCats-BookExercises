package cats.chapter3

import cats.Functor
import cats.instances.function._ // for Functor
import cats.syntax.functor._

object PartialUnificationApp extends App {

  val func1 = (x: Int) => x.toDouble + 1
  val func2 = (x: Double) => x * 2

  val func3 = func1.map(func2)
  val func3a: Int => Double = a => func2(func1(a))
  val func3b: Int => Double = func2.compose(func1)

  println(func3(1))
  println(func3a(1))
  println(func3b(1))

}
