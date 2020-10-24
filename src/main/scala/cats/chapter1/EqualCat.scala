package cats.chapter1

import java.util.Date

import cats.Eq
import cats.chapter1.TypeClass.{JsNumber, JsObject, JsString, Json, JsonWriter}
import cats.instances.int._
import cats.instances.long._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._

object EqualCat extends App {
//  println(List(1, 2, 3).map(Option(_)).filter(item => item.get == 1))

  val eqInt = Eq[Int]
  println(eqInt.eqv(5, 10))
  println(eqInt.eqv(-5, 5))
  println(eqInt.eqv(5, 5))

  println(5 =!= 10)
  println(-5 =!= 5)
  println(5 === 5)

  println("🤯🤯🤯" === "🤯🤯🤯")

  println((Some(1): Option[Int]) === (None: Option[Int]))
  println(1.some =!= none[Int])

  implicit val dateEq: Eq[Date] = Eq.instance[Date]((d1, d2) => d1.getTime === d2.getTime)

  val x = new Date()
  println("======")
  println(x === x)
  val y = new Date()
  println(y === x)
  println(y.getTime)
  println(x.getTime)

  class Animal()

//  object Animal {
//    implicit val animalWriter: JsonWriter[Animal] = new JsonWriter[Animal] {
//      def write(value: Animal): Json = JsObject(Map())
//    }
//  }

  implicit val animalWriter: JsonWriter[Animal] = new JsonWriter[Animal] {
    def write(value: Animal): Json = JsObject(Map())
  }

  final case class Cat(name: String, age: Int, color: String) extends Animal

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val cat3 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = Eq.instance((cat1, cat2) => cat1 == cat2)
  println("==================")
  println(cat1 === cat1)
  println(cat1 === cat2)
  println(cat3 === cat2)
  println("==================")
  println(optionCat1 === optionCat1)
  println(optionCat1 === optionCat2)

  // Eq[Option[Animal]] <- Eq[Some[Animal]] --> Covariance
  // Eq[Option[Animal]] <- Eq[None[Animal]] --> Covariance
  // to enable this we need Option[+A]

  // covariance is used for outputs
  // covariance is `+`
  // F[B] -> F[A] & B -> A

  // contravariance is `-`
  // F[B] -> F[A] & A -> B

  val animal: Animal = new Animal()
  val cat: Cat       = cat1


  // B -> A & F[A]
  def format[A](value: A)(writer: JsonWriter[A]): Json = writer.write(value)

  def m (in: Option[Animal]): Unit = ()

//  m(Option(cat))

  implicit val catWriter: JsonWriter[Cat] = new JsonWriter[Cat] {
    def write(value: Cat): TypeClass.Json = {
      JsObject(
        Map(
          "name"  -> JsString(value.name),
          "age"   -> JsNumber(value.age),
          "color" -> JsString(value.color)
        )
      )
    }
  }

  def printMyCat(writer: JsonWriter[Cat]): Unit = {
    println(writer.write(cat))
  }

  println("__________________")
  println(format(animal)(animalWriter))
  println(format(cat)(animalWriter))
  println(format(cat)(catWriter))

  printMyCat(animalWriter)
  printMyCat(catWriter)


//  println(animalWriter.write(cat))

//  val lst: List[Animal] = List(cat, cat ,cat) // covariance
  // animalWriter.write(cat) // contravariance

}
