package cats.chapter1

import cats.chapter1.TypeClass.JsonSyntax.JsonWriterOps
import cats.chapter1.TypeClass.JsonWriterInstances.{doubleWriter, intWriter, personWriter, stringWriter}

object TypeClass extends App {
  /*
  • traits: type classes;
  • implicit values: type class instances;
  • implicit parameters: type class use; and
  • implicit classes: optional utilities that make type classes easier to use
   */

  sealed trait Json
  final case class jsArray(get: List[Json])         extends Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String)            extends Json
  final case class JsNumber(get: Double)            extends Json
  final case object JsNull                          extends Json

  // type class
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  // A - parameter type

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      (value: String) => JsString(value)

    implicit val intWriter: JsonWriter[Int] =
      (value: Int) => JsNumber(value)

    implicit val doubleWriter: JsonWriter[Double] =
      (value: Double) => JsNumber(value)

    implicit val personWriter: JsonWriter[Person] =
      (value: Person) =>
        JsObject(
          Map(
            "name"  -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
  }

  println(Json.toJson(Person("Dave", "dave@example.com")))

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
    }
  }

  println(Person("Alex", "kuzmordas@rambler.ru").toJson)

  println(implicitly[JsonWriter[String]])
  println(implicitly[JsonWriter[Person]])

  println(Json.toJson("Hello World"))

  /*
    implicits scopes:
    1. by placing them in an object such as JsonWriterInstances;
    2. by placing them in a trait;
    3. by placing them in the companion object of the type class;
    4. by placing them in the companion object of the parameter type.
   */

  // TODO: investigate implicit scopes and its priorities

  /*
    creating type classes:
    1. by creating implicit vals
    2. by defining implicit methods which will return vals
   */

  implicit def listWriter[A](implicit writer: JsonWriter[A]): JsonWriter[List[A]] =
    new JsonWriter[List[A]] {
      def write(value: List[A]): Json = {
        jsArray(value.map(a => writer.write(a)))
      }
    }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
    case Some(a) => writer.write(a)
    case None    => JsNull
  }

  println(Option(123.1).toJson)
  println(Option(123).toJson)
  println(Option("123").toJson)
  println(Option(Person("artem", "artgorev@gmail.com")).toJson)
  println(Option(List(1, 2, 3, 4)).toJson)
}
