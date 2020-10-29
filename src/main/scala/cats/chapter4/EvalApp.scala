package cats.chapter4

import cats.{Eval, Monoid}
import cats.instances._
import cats.syntax.monoid._

object EvalApp extends App {
  val x = {
    println("Computing X")
    math.random()
  }

  def y = {
    println("Computing Y")
    math.random()
  }

  lazy val z = {
    println("Computing Z")
    math.random()
  }

  println(x)
  println(x)
  println(y)
  println(y)
  println(z)
  println(z)

  val now = Eval.now {
    println("computing now")
    math.random()
  }
  val later = Eval.later {
    println("computing later")
    math.random()
  }
  val always = Eval.always {
    println("computing always")
    math.random()
  }
  print("\n\n\n")
  println(s"now ${now}")
  println(s"now ${now.value}")
  println(s"always ${always.value}")
  println(s"always ${always.value}")
  println(s"later ${later}")
  println(s"later ${later.value}")
  print("\n\n\n")

  val greeting = Eval
    .always { println("Step 1"); "Hello" }
    .map { str => println("Step 2"); s"$str world" }

  println(greeting.value)

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }
  println(ans.value)

  implicit val evalMonoid = new Monoid[Eval[Int]] {
    override def empty: Eval[Int] = Eval.now(0)

    override def combine(x: Eval[Int], y: Eval[Int]): Eval[Int] = Eval.now(x.value + y.value)
  }

  val a = Eval.now { println("Calculating A"); 40 }
  val b = Eval.always { println("Calculating B"); 2 }
  println("Adding A and B")
  println((a |+| b).value)

  val saying = Eval
    .always { println("Step 1"); "The cat" }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }

  saying.value // first access
  saying.value // second access

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.later(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(10).value)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case Nil => Eval.now(acc)
      case head :: tail =>
        Eval
          .defer(
            foldRight(tail, acc)(fn)
              .map(fn(head, _))
          )
    }

  // [C] -> [C] -> [C] -> [C] -> [C] ->
  //  *[C] ->*[C] ->*[C] ->*[C] ->

  private val list = (0 to 100000).toList
  val yyy: String  = foldRight(list, "")((a, b) => b + a.toString).value
  println(yyy)
}
