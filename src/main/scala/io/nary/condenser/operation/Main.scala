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
package operation

import scalaz.stream.{Process, Sink, sink, channel}
import scalaz.concurrent.Task

import scalaz._
import Scalaz._

import org.http4s.server.blaze.BlazeBuilder

import argonaut.Json
import squants.time.{Milliseconds, Seconds, Minutes, Hours}

import defs._
import stats._
import threading._

/**
 * The Entry-point for the Condenser which loads all of the necessary emoji data, starts the Tweet stream, 
 * as well as performing inline and background data processing.  Here we also startup the HTTP Service
 * for issuing Requests to the condenser for real-time updates and diagnostic purposes.
 */
object Main {
  
  // Used for computing tweet velocities (per hour, minute, and second)
  case class IntermediateStats(count: Long, timeAccum: TimeAccumulator)
  
  def main(args: Array[String]) {
    if(args.length != 4)
      println("Invalid Usage: expected arguments <consumer key> <consumer secret> <access token> <access secret>")
    else {
      val consumerKey = args(0)
      val consumerSecret = args(1)
      val accessToken = args(2)
      val accessSecret = args(3)
      
      println("Loading Emoji Data ...")

      val emojis = (for {
        e <- Emoji.maps
        _ <- CurrentStats.emojiDefinitionSize.set(e._1.size -> e._2.size)
      } yield e).run

      println("Starting Background processor.")

      runForked(Background.process(emojis._1, emojis._2).run)

      println("Streaming Tweets Now!")
      
      // Here we start the twitter stream, processing tweet counts and velocities inline
      // Note: one can easily delegate this to the Background process.

      runForked(Twitter.from(consumerKey, consumerSecret, accessToken, accessSecret)
        .observe(tweetTextSink)
        .observe(tweetJsonSink)
        .fold(IntermediateStats(0, initialTimeAccum(Milliseconds(System.currentTimeMillis))))
        { case (interm, json) => 
            val n = interm.count + 1
            val ta = interm.timeAccum + Milliseconds(System.currentTimeMillis)
            (for {
              _ <- CurrentStats.tweetCount.set(n)
              _ <- CurrentStats.tweetVelocity.set(ta.averageVelocity)
            } yield IntermediateStats(n, ta)).run
        }.run)

      println("HTTP Service Running.")
      serviceBuilder.run.awaitShutdown()
    }
  }

  /* 
   * Builds the http4s service 
   */
  def serviceBuilder : BlazeBuilder = {
    import org.http4s.server.blaze._
    import org.http4s.server.syntax._

    BlazeBuilder.bindHttp(9000, "localhost").mountService(CondenserHttp.service, "/")
  }

  // These sinks serve as away for caching JSON strings (for later review) and Tweet JSON content, for the background processor
  def tweetTextSink : Sink[Task, Json] = sink.lift(json => CurrentStats.tweetStrings.enqueueOne(json.toString))
  def tweetJsonSink : Sink[Task, Json] = sink.lift(json => Background.tweetQueue.enqueueOne(json))  
}