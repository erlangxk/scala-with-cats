package mx.study

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object Ex4_7_3 extends App {

  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    println(s"step $n")
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  //  factorial(5)

  type Logged[A] = Writer[Vector[String], A]

  def fact(n: Int): Logged[Int] = {
    for {
      _ <- Vector(s"step $n").tell
      ans <- slowly(if (n == 0) 1.pure[Logged] else fact(n - 1).map(_ * n))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  //  val r = fact(5)
  //  println(r.run)


  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(Future(factorial(3)), Future(factorial(3)))), 5.seconds)

  val rr = Await.result(Future.sequence(Vector(Future(fact(3).run), Future(fact(3).run))), 5.seconds)
  println(rr)

}
