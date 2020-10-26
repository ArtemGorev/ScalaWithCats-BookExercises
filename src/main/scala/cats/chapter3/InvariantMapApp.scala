package cats.chapter3

import cats.chapter3.ContraMapApp.Printable

object InvariantMapApp extends App {
  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))

      def decode(value: String): B = dec(self.decode(value))
    }
  }
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }
  implicit val intCodec: Codec[Int]         = stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  object Codec {
    def encode[A](value: A)(implicit w: Codec[A]): String = w.encode(value)
    def decode[A](value: String)(implicit w: Codec[A]): A = w.decode(value)
  }

  println(encode(123))
  println(decode[String]("HELLO"))
  println(decode[Int]("123"))
  println(encode[Boolean](true))
  println(decode[Boolean]("true"))

  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)
  println(encode[Double](123.1) + "123")
  println(decode[Double]("33.3") + 44.4)

  final case class Box[A](var value: A)
  implicit def boxCodec[A](implicit codec: Codec[A]): Codec[Box[A]] =
    codec.imap(value => Box[A](value), value => value.value)

  val value: Box[Double] = decode[Box[Double]]("33.3")
  println(value.value + 22.2)

  value.value = 22.2 + value.value
  println(encode[Box[Double]](value))

  println(encode(Box(123.4)))
  // res13: String = "123.4"
  println(decode[Box[Double]]("123.4"))
  // res14: Box[Double] = Box(123.4)

}
