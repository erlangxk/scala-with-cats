package mx.study.cf

import cats.effect.{Async, IO, SyncIO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object MxAsync extends  App{

  val apiCall = Future.successful("I come from the future")

  val ioa:IO[String] = Async[IO].async{ cb=>
    apiCall.onComplete{
      case Success(value) => cb(Right(value))
      case Failure(error) => cb(Left(error))

    }
  }

  val x:SyncIO[Unit] = ioa.runAsync{
    case Right(x) => IO(println("ok " + x))
    case Left(err) => IO(println("err"))
  }

  x.unsafeRunSync()

  Thread.sleep(2000)

}
