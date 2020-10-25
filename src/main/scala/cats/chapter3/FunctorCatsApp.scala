package cats.chapter3

import cats.Functor
import cats.instances.list._   // for Functor
import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._

object FunctorCatsApp extends App {

  val list1 = List(1, 2, 3)
  def f(x: Int) =  s"result: ${x * 2}"
  val list2 = Functor[List].map(list1)(f)
  println(list1)
  println(list2)

  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  println(option1)
  println(option2)

  val liftedFunc = Functor[Option].lift(f)
  println(liftedFunc(option1))

  val asFunctor = Functor[Option].as(option1, "bla-bla-bla")
  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"$a!"
  val func4 = func1.map(func2).map(func3)
  println(func4(123))

  implicit val boxFunctor: Functor[Box] = new Functor[Box]() {
    def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }

  def ff(x: Int) = s"$x + 1"
  final case class Box[A](value: A)
  val box = Box(123)
  println(box.map(ff))
  val liftedFf = Functor[Box].lift(ff)
  println(liftedFf(box))
  println(box.as("123!"))


}
