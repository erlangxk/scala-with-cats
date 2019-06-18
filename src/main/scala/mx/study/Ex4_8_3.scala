package mx.study

import cats.data.Reader;

object Ex4_8_3 extends App {

  case class DB(usernames: Map[Int, String], passwords: Map[String, String])

  type DBReader[A] = Reader[DB, A]

  def findUsername(userId: Int): DBReader[Option[String]] = Reader { db =>
    db.usernames.get(userId)
  }

  def checkPassword(username: String, password: String): DBReader[Boolean] = Reader { db =>
    db.passwords.get(username).exists(_ == password)
  }

  def checkLogin(userId: Int, password: String): DBReader[Boolean] =
    findUsername(userId).flatMap {
      case None => Reader(_ => false)
      case Some(n) => checkPassword(n, password)
    }
}
