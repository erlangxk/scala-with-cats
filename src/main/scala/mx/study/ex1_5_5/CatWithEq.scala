package mx.study.ex1_5_5

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catEq: Eq[Cat] = Eq.instance { (c1, c2) =>
    c1.name === c2.name && c1.age === c2.age && c1.color === c2.color
  }
}

object CatWithEq extends App {

  val c1 = Cat("Garfield", 38, "orange and black")
  val c2 = Cat("Heathcliff", 33, "orange and black")

  val opC1 = Option(c1)
  val opC2 = Option.empty[Cat]

  println(c1 === c2)
  println(c1 =!= c2)
  println(opC1 === opC2)
  println(opC1 =!= opC2)

  println(c1.some === none[Cat])
  println(c1.some =!= none[Cat])
}
