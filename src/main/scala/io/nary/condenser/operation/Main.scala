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

object Main {
  
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
      
      Twitter.from(consumerKey, consumerSecret, accessToken, accessSecret)
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
        }
        .run.runAsync(_ => ())

      println("HTTP Service Running.")
      serviceBuilder.run.awaitShutdown()
    }
  }

  def serviceBuilder : BlazeBuilder = {
    import org.http4s.server.blaze._
    import org.http4s.server.syntax._

    BlazeBuilder.bindHttp(9000, "localhost").mountService(CondenserHttp.service, "/")
  }

  def tweetTextSink : Sink[Task, Json] = sink.lift(json => CurrentStats.tweetStrings.enqueueOne(json.toString))
  def tweetJsonSink : Sink[Task, Json] = sink.lift(json => Background.tweetQueue.enqueueOne(json))  
}