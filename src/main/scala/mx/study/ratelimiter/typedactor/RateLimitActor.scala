package mx.study.ratelimiter.typedactor

import java.util.concurrent.TimeUnit

import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.scalalogging.StrictLogging
import mx.study.ratelimiter.{RateLimiterQueue, Run, RunAfter}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration

sealed trait RateLimiterMsg

case class LazyFuture(f: () => Future[Unit]) extends RateLimiterMsg

case object ScheduleRunQueue extends RateLimiterMsg

class RateLimiter(actorSystem: ActorSystem[RateLimiterMsg]) extends StrictLogging {
  def runLimited[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val p = Promise[T]
    actorSystem ! LazyFuture(() => f.andThen({ case r => p.complete(r) }).map(_ => ()))
    p.future
  }

  def stop(): Future[Unit] = {
    import actorSystem.executionContext
    val f = actorSystem.whenTerminated.map(_ => logger.info("Stopping rate limiter"))
    actorSystem.terminate()
    f
  }
}

object RateLimiter {
  def create(maxRuns: Int, per: FiniteDuration): RateLimiter = {
    val behavior = Behaviors.withTimers[RateLimiterMsg] { timer =>
      rateLimit(timer, RateLimiterQueue(maxRuns, per.toMillis))
    }
    new RateLimiter(ActorSystem(behavior, "rate-limiter"))
  }

  def rateLimit(timer: TimerScheduler[RateLimiterMsg], queue: RateLimiterQueue[LazyFuture]): Behavior[RateLimiterMsg] = Behaviors.receiveMessage {
    case lf: LazyFuture => rateLimit(timer, runQueue(timer, queue.enqueue(lf)))
    case ScheduleRunQueue => rateLimit(timer, runQueue(timer, queue.notScheduled))
  }

  def runQueue(timer: TimerScheduler[RateLimiterMsg], queue: RateLimiterQueue[LazyFuture]): RateLimiterQueue[LazyFuture] = {
    val now = System.currentTimeMillis()
    val (tasks, queue2) = queue.run(now)
    tasks.foreach {
      case Run(LazyFuture(f)) => f()
      case RunAfter(millis) => timer.startSingleTimer((), ScheduleRunQueue, FiniteDuration(millis, TimeUnit.MILLISECONDS))
    }
    queue2
  }
}
