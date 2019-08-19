package mx.study.ratelimiter

import scala.collection.immutable.Queue

sealed trait RateLimiterTask[F]

case class Run[F](run: F) extends RateLimiterTask[F]

case class RunAfter[F](millis: Long) extends RateLimiterTask[F]

// at perMillis time window, at most maxRuns computations are started.
case class RateLimiterQueue[F](maxRuns: Int, perMillis: Long, lastTimestamp: Queue[Long], waiting: Queue[F], scheduled: Boolean) {
  def enqueue(f: F): RateLimiterQueue[F] = copy(waiting = waiting.enqueue(f))

  def notScheduled: RateLimiterQueue[F] = copy(scheduled = false)

  private def doRun(now: Long): (List[RateLimiterTask[F]], RateLimiterQueue[F]) = {
    if (lastTimestamp.size < maxRuns) {
      waiting.dequeueOption match {
        case Some((e, rest)) =>
          val rlQ = copy(lastTimestamp = lastTimestamp.enqueue(now), waiting = rest)
          val (tasks, next) = rlQ.run(now)
          (Run(e) :: tasks, next)
        case None =>
          (Nil, this)
      }
    } else if (!scheduled) {
      val nextAvailableSlot = perMillis - (now - lastTimestamp.head)
      (List(RunAfter(nextAvailableSlot)), this.copy(scheduled = true))
    } else {
      (Nil, this)
    }
  }

  private def pruneTimeStamps(now: Long): RateLimiterQueue[F] = {
    val threshold = now - perMillis
    copy(lastTimestamp = lastTimestamp.filter(_ >= threshold))
  }

  def run(now: Long): (List[RateLimiterTask[F]], RateLimiterQueue[F]) = {
    pruneTimeStamps(now).doRun(now)
  }

}

object RateLimiterQueue {
  def apply[F](maxRuns: Int, perMillis: Long): RateLimiterQueue[F] = RateLimiterQueue(maxRuns, perMillis, Queue.empty, Queue.empty, false)
}
