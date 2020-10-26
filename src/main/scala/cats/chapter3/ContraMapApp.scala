package cats.chapter3

object ContraMapApp extends App {

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
  }

  implicit val stringPrintable: Printable[String] = (value: String) => s"'$value'"
  implicit val intPrintable: Printable[Int]       = (value: Int) => s"$value"

  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit printer: Printable[A]): Printable[Box[A]] =
    printer.contramap((value: Box[A]) => value.value)

  object Formatter {
    def format[A](value: A)(implicit w: Printable[A]): String = w.format(value)
  }

  println(Formatter.format("hello"))
  // res2: String = "'hello'"
  println(Formatter.format(true))
  // res3: String = "yes"
  println(Formatter.format(Box("hello world")))
  println(Formatter.format(Box(true)))
  println(Formatter.format(Box(false)))
  println(Formatter.format(12345))
}
