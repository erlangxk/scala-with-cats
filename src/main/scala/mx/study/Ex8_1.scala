package mx.study

import cats.{Applicative, Id}

import scala.concurrent.Future
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import scala.language.higherKinds

object Ex8_1 extends  App{

  trait UpTimeClient[F[_]]{
    def getUptime(host:String):F[Int]
  }

  trait RealUpTimeClient extends UpTimeClient[Future] {
    def getUptime(host:String):Future[Int]
  }

  trait TestUpTimeClient extends UpTimeClient[Id]{
    def getUptime(host:String):Id[Int]
  }

  class UptimeService[F[_]:Applicative](client: UpTimeClient[F]) {
    def getTotalUptime(hosts: List[String]): F[Int] =
      hosts.traverse(client.getUptime).map(_.sum)
  }

}
