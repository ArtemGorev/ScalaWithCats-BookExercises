package archive.chapter1

import cats.Eq
import cats.implicits.catsSyntaxOptionId
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._

final case class Cat(name: String, age: Int, color: String)


object ExEq extends App {
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
  }

  println(cat1 === cat1)
  println(cat1 === cat2)
  println(cat1 =!= cat2)
  println(cat2 =!= cat2)

  // option

  println((optionCat1: Option[Cat]) === (Some(cat1) : Option[Cat]))
  println(optionCat1 === Some(cat1))
  println(optionCat1 === cat1.some)
  println(optionCat1 === none[Cat])
  println(optionCat1 =!= none[Cat])

  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
}
