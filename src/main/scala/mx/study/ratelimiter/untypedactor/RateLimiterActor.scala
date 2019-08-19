package mx.study.ratelimiter.untypedactor

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import mx.study.ratelimiter.{RateLimiterQueue, Run, RunAfter}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.{Duration, FiniteDuration}

case class LazyFuture(t: () => Future[Unit])

case object ScheduleRunQueue

class RateLimiterActor(maxRuns: Int, perMillis: FiniteDuration) extends Actor with ActorLogging {

  import context.dispatcher

  private var queue = RateLimiterQueue[LazyFuture](maxRuns, perMillis.toMillis)

  def runQueue() = {
    val now = System.currentTimeMillis()
    val (tasks, queue2) = queue.run(now)
    queue = queue2
    tasks.foreach {
      case Run(LazyFuture(f)) => f()
      case RunAfter(millis) => context.system.scheduler.scheduleOnce(Duration(millis, TimeUnit.MILLISECONDS), self, ScheduleRunQueue)
    }
  }

  override def receive = {
    case lf: LazyFuture =>
      queue = queue.enqueue(lf)
      runQueue()

    case ScheduleRunQueue =>
      queue = queue.notScheduled
      runQueue()
  }
}

class AkkaRateLimiter(rateLimiterActor: ActorRef) {
  def runLimited[T](f: Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val p = Promise[T]
    val msg = LazyFuture(() => f.andThen { case r => p.complete(r) }.map(_ => ()))
    rateLimiterActor ! msg
    p.future
  }
}

object AkkaRateLimiter {
  def create(maxRuns: Int, per: FiniteDuration)(implicit actorSystem: ActorSystem): AkkaRateLimiter = {
    val rateLimiterActor = actorSystem.actorOf(Props(new RateLimiterActor(maxRuns, per)))
    new AkkaRateLimiter(rateLimiterActor)
  }
}
