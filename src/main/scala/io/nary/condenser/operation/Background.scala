package io.nary.condenser
package operation

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}
import scala.concurrent.ExecutionContext

import scalaz.~>
import scalaz.stream.{Channel, Process, Sink, Writer, channel, sink, time}
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import scalaz.stream.async._
import scalaz.stream.async.mutable._

import argonaut.Json

import defs._

object Background {
    
    val tweetQueue : Queue[Json] = circularBuffer[Json](1000)
    
    def process(asciiEmojiMap: Map[String, EmojiDef], unicodeHexEmojiMap: Map[String, EmojiDef]) : Process[Task, Unit] = 
      tweetQueue.dequeue.flatMap { json =>
        Process.eval(
          for {
            data <- Twitter.tweetData(json)(asciiEmojiMap, unicodeHexEmojiMap)
            _ <- CurrentStats.tweetData.enqueueOne(data)
          } yield ()
        )
      }
} 