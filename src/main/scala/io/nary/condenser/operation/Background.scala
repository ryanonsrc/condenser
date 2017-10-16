package io.nary.condenser
package operation

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService, ThreadFactory}
import scala.concurrent.ExecutionContext

import scalaz._
import Scalaz._

import scalaz.stream.{Channel, Process, Sink, Writer, channel, sink, time}
import scalaz.concurrent.Task
import scalaz.syntax.bind._
import scalaz.stream.async._
import scalaz.stream.async.mutable._

import argonaut.Json

import defs._

object Background {
    
    val tweetQueue : Queue[Json] = circularBuffer[Json](1000)
    
    import Ranking._
    import Parsing._

    def process(asciiEmojiMap: Map[String, EmojiDef], unicodeHexEmojiMap: Map[String, EmojiDef]) : Process[Task, Unit] = 
      tweetQueue.dequeue.flatMap { json =>
        Process.eval(
          for {
            data <- Twitter.tweetData(json)(asciiEmojiMap, unicodeHexEmojiMap)
            
            hasEmoji <- Task.now(data.emoji.size > 0)
            hasUrls <- Task.now(data.Urls.size > 0)
            hasPicUrls <- Task.now(data.PicUrls.size > 0)

            tcWithEmojis <- CurrentStats.tweetCountWithEmojis.get
            tcWithUrls <- CurrentStats.tweetCountWithUrls.get
            tcWithPhotoUrls <- CurrentStats.tweetCountWithPhotoUrls.get
            
            emojiRank <- CurrentStats.topEmojis.get.map(_.pruneWhen(1000)).map { r => 
              data.emoji.foldLeft(r) { case (erank, e) => erank + e }
            }
            hashtagRank <- CurrentStats.topHashtags.get.map(_.pruneWhen(1000)).map { r =>
              data.hashTags.foldLeft(r) { case (hrank, h) => hrank + h }
            }
            urlDomainRank <- CurrentStats.topUrlDomains.get.map(_.pruneWhen(1000)).map { r =>
              data.Urls.map(extractDomain).foldLeft(r) { case (drank, d) => drank + d }
            }
            
            _ <- hasEmoji.fold(CurrentStats.tweetCountWithEmojis.set(tcWithEmojis + 1), Task.now(()))
            _ <- hasUrls.fold(CurrentStats.tweetCountWithUrls.set(tcWithUrls + 1), Task.now(()))
            _ <- hasPicUrls.fold(CurrentStats.tweetCountWithPhotoUrls.set(tcWithPhotoUrls + 1), Task.now(()))

            _ <- CurrentStats.tweetData.enqueueOne(data)
            _ <- CurrentStats.topEmojis.set(emojiRank)
            _ <- CurrentStats.topHashtags.set(hashtagRank)
            _ <- CurrentStats.topUrlDomains.set(urlDomainRank)
          } yield ()
        )
      }
} 