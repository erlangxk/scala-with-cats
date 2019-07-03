package mx.study

import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.either._

case class User(name: String, age: Int)

object Ex6_4_4 extends App {

  def getValue(field: String, map: Map[String, String]): Either[List[String], String] = {
    map.get(field) match {
      case None => Left(List(s"$field not found"))
      case Some(n) => Right(n)
    }
  }

  def parseInt(s: String) = {
    try {
      Right(s.toInt)
    } catch {
      case _: NumberFormatException => Left(List("age is not a number"))
    }
  }

  def nonBlank(s: String) = {
    if (s.isEmpty) Left(List("empty string")) else Right(s)
  }

  def nonNegative(n: Int) = {
    if (n > 0) Right(n) else Left(List("negative number"))
  }

  def readName(params: Map[String, String]): Either[List[String], String] = {
    for {
      n <- getValue("name", params)
      nonBs <- nonBlank(n)
    } yield nonBs
  }

  def readAge(params: Map[String, String]): Either[List[String], Int] = {
    for {
      age <- getValue("age", params)
      a1 <- parseInt(age)
      a2 <- nonNegative(a1)
    } yield a2
  }

  def readUser(params: Map[String, String]): Either[List[String], User] = {
    (readName(params).toValidated, readAge(params).toValidated).mapN(User.apply).toEither
  }

  println(readUser(Map("age" -> "-1")))

}
