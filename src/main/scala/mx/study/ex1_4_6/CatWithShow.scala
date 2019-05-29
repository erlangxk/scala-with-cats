package mx.study.ex1_4_6

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catShow: Show[Cat] = Show.show { cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat" }
}

object CatWithShow extends App {
  val cat = Cat("miao", 3, "brown")
  println(cat.show)
}
