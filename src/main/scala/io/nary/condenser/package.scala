/***********************************************************************
Copyright 2017 (C)  Ryan Delucchi  [nary.io || fp.engineering]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/

package io.nary.condenser

import scalaz.{Kleisli, Reader, Applicative}
import argonaut.Json

import squants.time.{Time, Hours, Minutes, Seconds, Milliseconds}
import squants.{Quantity, Ratio, Each, Dimension, Dimensionless}

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}

import scalaz._
import Scalaz._

import scalaz.concurrent.{Task, Strategy}

/**
 * Fundamental data definitions are placed here.  The "Chunks" type-abstraction is a particularly neat way to model 
 * an Effect Stream, Effect and Output values which maps directly onto the scalaz-stream Process, Task and output constructs.
 * The TimeAccumulator is used for computing averages (which we need for tweet velocity calculation).
 */
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

/**
 * Here we have everything we need for computing tweet velocities.  Our work here is made more clear
 * by use of the squants library, which allows us to perform computations that include units of measure.
 * we probably didn't need to define any supporting ADTs, but due to some oddities to the squants library
 * it was helpful seperate out some of the units and combine them mostly for computing time ratios.
 */
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

/**
 * Here we have some fairly standard machinery for defining our thread-pools, executors, etc.
 */
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