package archive.chapter2

import cats.data.State
import State._

object StateMonad extends App {
  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

//  val (state, result) = a.run(10).value
//  println(s"state $state result $result")

  val justTheState = a.runS(10).value
  val justTheResult = a.runA(10).value



  val step1 = State[Int, String](num => {
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  })

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = step1.flatMap(a => step2.map(b => (a, b)))

  val (state, result) = both.run(20).value
  println(s"state $state result $result")


  val getDemo = State.get[Int]
  println(getDemo.run(10).value)

  val setDemo = State.set[Int](30)
  println(setDemo.run(10).value)

  val pureDemo = State.pure[Int, String]("Result")
  println(pureDemo.run(10).value)

  val inspectDemo = State.inspect[Int, String]( x => s"${x}!")
  println(inspectDemo.run(20).value)

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(1).value)


  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  // program: State[Int, (Int,
  val (state1, result1) = program.run(1).value
  println(state1, result1)
}
