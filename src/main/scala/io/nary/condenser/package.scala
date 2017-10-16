package io.nary.condenser

import scalaz.{Kleisli, Reader, Applicative}
import argonaut.Json

import squants.time.{Time, Hours, Minutes, Seconds, Milliseconds}
import squants.{Quantity, Ratio, Each, Dimension, Dimensionless}

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}

import scalaz._
import Scalaz._

import scalaz.concurrent.{Task, Strategy}

package object defs {
  type Chunks[G[_[_], _], F[_], A] = G[F, A]

  case class TimeAccumulator(timestamp: Time, sum: Time, count: Int)

  case class AverageVelocity(perHour: Double, perMinute: Double, perSecond: Double)
  case class EmojiDef(name: String, ascii: List[String], unicodeHex: Option[String])
  case class Hashtag(text: String)
  case class TweetUrl(url: String)
  case class TweetData(text: String, emoji: List[EmojiDef], hashTags: List[Hashtag], Urls: List[TweetUrl], PicUrls: List[TweetUrl])

  def ignoredTweet = TweetData("", Nil, Nil, Nil, Nil)  
}

package object stats {

  import defs._

  def initialTimeAccum(timestamp: Time) = TimeAccumulator(timestamp, Milliseconds(0), 1)

  implicit class TimeAccumulatorOps(val accum : TimeAccumulator) extends AnyVal {
    def +(timestamp: Time) : TimeAccumulator = accum.copy(timestamp = timestamp, 
      sum = accum.sum + (timestamp - accum.timestamp), count = accum.count + 1)

    def average : Time = accum.sum / accum.count
    
    def averageVelocity : AverageVelocity = {
      val avg = average
      AverageVelocity(perHour = Hours(1) / avg, perMinute = Minutes(1) / avg, perSecond = Seconds(1) / avg)
    }
  }

  

}

package object threading {
  def schedulingExecutor: Strategy = Strategy.Executor(schedulingPool)

  def daemonThreads(name: String) = new ThreadFactory {
    def newThread(r: Runnable) = {
      val t = Executors.defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t.setName(name)
      t
    }
  }

  def schedulingPool: ScheduledExecutorService =
      Executors.newScheduledThreadPool(4, daemonThreads("scheduled-tasks"))

  def runForked[A](t: Task[A]) : Unit = Task.fork(t)(schedulingPool).runAsync {
    case -\/(th) => th.printStackTrace()
    case \/-(_) => println("Terminated without exception.")
  }    
}