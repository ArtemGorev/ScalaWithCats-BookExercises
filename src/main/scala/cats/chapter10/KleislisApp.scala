package cats.chapter10
import cats.data.Kleisli
import cats.instances.list._

object KleislisApp extends App {

  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  println(step1.run(1))
  println(step2.run(1))
  println(step3.run(1))

  val pipeline = step1.andThen(step2).andThen(step3)

  println(step1.andThen(step2).run(1))
  println(pipeline.run(1))

}
