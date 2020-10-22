package cats.chapter1

import cats.Show
import cats.instances.int._
import cats.instances.string._
//import cats.instances.list._
import cats.syntax.show._

object MeetCats extends App {
  final case class Cat(name: String, age: Int, color: String)

  implicit def listPrint[A](implicit p: Show[A]): Show[List[A]] = {
    new Show[List[A]] {
      def show(x: List[A]): String = x.map(y => p.show(y)).mkString("\n")
    }
  }

  val showInt: Show[Int]        = Show.apply[Int]
  val showString: Show[String]  = Show.apply[String]
  //  val showList: Show[List[Int]] = Show.apply[List[Int]]

  //  println(showInt.show(123))
  //  println(showString.show("abc"))
  //  println(456.show)
  //  println("def".show)
  //  println(List(1,2,3).show)

  //  implicit val dateShow: Show[Date] =
  //    new Show[Date] {
  //      def show(t: Date): String = s"${t.getTime}ms since the epoch."
  //    }

  //  implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")
  //  implicit val dateShow: Show[Date] = Show.fromToString
  //  println(new Date().show)

  implicit val catShow: Show[Cat] = Show.show(cat => s"""${cat.name} is a ${cat.age} year-old ${cat.color} cat.""")
  val catsLst = List(Cat("Barsik", 1, "black"), Cat("Barsik", 2, "red"), Cat("Barsik", 3, "white"))
  println(catsLst.show)
}
