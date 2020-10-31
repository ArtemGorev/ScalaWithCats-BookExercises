package cats.chapter4
import cats.data.State
import State._

object StateMonadApp extends App {
  val a = State[Int, String] { state => (state, s"The state is $state") }
  println(a.run(10).value)
  println(a.runS(10).value)
  println(a.runA(10).value)

  //
  val step1: State[Int, String] = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }
  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }
  val both = step1.flatMap((a: String) => step2.map((b: String) => (a, b)))
  println(both.run(20).value)

  println(s"""//\\""")
  println(State.get[Int].run(10).value)

  val setDemo = State.set[Int](30)
  println(setDemo.run(10).value)

  val pureDemo = State.pure[Int, String]("Hello World")
  println(pureDemo.run(101).value)

  val inspectDemo = State.inspect[Int, String](x => s"$x!")
  println(inspectDemo.run(1).value)

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(10).value)

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  // program: State[Int, (Int, Int, Int)] = cats.data.
  println(program.run(1).value)
}
