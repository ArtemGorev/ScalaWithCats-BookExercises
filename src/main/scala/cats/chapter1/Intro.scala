package cats.chapter1

import cats.chapter1.Intro.PrintableInstances.{listPrint, printableCat, printableInt, printableString}
import cats.chapter1.Intro.PrintableSyntax.PrintableOps

object Intro extends App {

  final case class Cat(name: String, age: Int, color: String)

  trait Printable[A] {
    def format(x: A): String
  }

  object PrintableInstances {
    implicit val printableString: Printable[String] = new Printable[String] {
      def format(x: String): String = x
    }

    implicit val printableInt: Printable[Int] = new Printable[Int] {
      def format(x: Int): String = x.toString
    }

    implicit val printableCat: Printable[Cat] = new Printable[Cat] {
      def format(x: Cat): String = s"""${x.name} is a ${x.age} year-old ${x.color} cat."""
    }

    implicit def listPrint[A](implicit p: Printable[A]): Printable[List[A]] = {
      new Printable[List[A]] {
        def format(x: List[A]): String = x.map(y => p.format(y)).mkString("\n")
      }
    }
  }

  object Printable {
    def format[A](x: A)(implicit p: Printable[A]): String = p.format(x)
    def print[A](x: A)(implicit p: Printable[A]): Unit = println(format(x))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def print(implicit p: Printable[A]): Unit = {
        Printable.print(value)
      }
    }
  }

  val cat = Cat("Barsik", 1, "black-white")
  cat.print

  val lst = List(1, 2, 3, 4, 5)
  lst.print

  val catsLst = List(Cat("Barsik", 1, "black"), Cat("Barsik", 2, "red"), Cat("Barsik", 3, "white"))
  catsLst.print
}
