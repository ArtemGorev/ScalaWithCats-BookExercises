package cats.chapter4

import cats.data.State
import cats.data.State._

object StateMonadExerciseApp extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym(0) match {
    case s if Character.isDigit(s) =>
      for {
        _   <- State.modify[List[Int]](list => s.asDigit :: list)
        ans <- State.get[List[Int]]
      } yield ans.head

    case s if s == '+' =>
      for {
        _   <- State.modify[List[Int]](list => (list.head + list.tail.head) :: list.tail.tail)
        ans <- State.get[List[Int]]
      } yield ans.head

    case s if s == '*' =>
      for {
        _   <- State.modify[List[Int]](list => (list.head * list.tail.head) :: list.tail.tail)
        ans <- State.get[List[Int]]
      } yield ans.head
  }

  println(evalOne("1").run(List()).value)

  val program = evalOne("1")
    .flatMap(a =>
      evalOne("2")
        .flatMap(b =>
          evalOne("+")
            .flatMap(c =>
              evalOne("2")
                .flatMap(d =>
                  evalOne("*")
                    .map(ans => (a, b, c, d, ans))
                )
            )
        )
    )

  println(program.run(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0))((acc, v) => acc.flatMap(_ => evalOne(v)))

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))

  println(multistageProgram.run(Nil).value)

  val biggerProgram = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(biggerProgram.run(Nil).value)

  def evalInput(s: String): CalcState[Int] = evalAll(s.split(" ").toList)

  println(evalInput("1 2 + 3 + 4 * 5 +").run(Nil).value)
}
// State [Stack, Int]
