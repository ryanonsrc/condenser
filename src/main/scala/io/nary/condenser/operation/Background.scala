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

/**
 * The Background Processer allows further statistics to be gathered, independantly of the main stream, 
 * for improved performance.  It should be noted that as we are utilizing a scalaz Circular Buffer for the 
 * Tweet Queue that feeds the background process, we can avoid a memory leak situation if background processing
 * is moving along more slowely than the Twitter stream.  Scaling up the Condenser can be achieved by adding additional
 * queues here, running parts of this process in parallel, and offloading more functionality that is occuring inline
 * of the primary Twitter stream and placing it here.  Of course, there is a balance that must be struck by whether
 * to utilize the background process for gathering a given stat, how large the circular buffer should be (and once again
 * if one wants to consider introducing additional buffers).
 */
object Background {
    
    val tweetQueue : Queue[Json] = circularBuffer[Json](1000)
    
    import Ranking._
    import Parsing._

    def process(asciiEmojiMap: Map[String, EmojiDef], unicodeHexEmojiMap: Map[String, EmojiDef]) : Process[Task, Unit] = 
      tweetQueue.dequeue.flatMap { json =>
        Process.eval(
          for {
            // Performs a wholesale extraction of data from the Tweet by cross-referencing the Emoji definition
            data <- Twitter.tweetData(json)(asciiEmojiMap, unicodeHexEmojiMap)
            
            // Stats on whether the tweet has emoji, or urls
            hasEmoji <- Task.now(data.emoji.size > 0)
            hasUrls <- Task.now(data.Urls.size > 0)
            hasPicUrls <- Task.now(data.PicUrls.size > 0)

            // stats on the number of tweets with these entity types
            tcWithEmojis <- CurrentStats.tweetCountWithEmojis.get
            tcWithUrls <- CurrentStats.tweetCountWithUrls.get
            tcWithPhotoUrls <- CurrentStats.tweetCountWithPhotoUrls.get
            
            // stats for ranking statistics
            emojiRank <- CurrentStats.topEmojis.get.map(_.pruneWhen(1000)).map { r => 
              data.emoji.foldLeft(r) { case (erank, e) => erank + e }
            }
            hashtagRank <- CurrentStats.topHashtags.get.map(_.pruneWhen(1000)).map { r =>
              data.hashTags.foldLeft(r) { case (hrank, h) => hrank + h }
            }
            urlDomainRank <- CurrentStats.topUrlDomains.get.map(_.pruneWhen(1000)).map { r =>
              data.Urls.map(extractDomain).foldLeft(r) { case (drank, d) => drank + d }
            }
            
            // only update these stats if we actually are increasing any of these counts
            _ <- hasEmoji.fold(CurrentStats.tweetCountWithEmojis.set(tcWithEmojis + 1), Task.now(()))
            _ <- hasUrls.fold(CurrentStats.tweetCountWithUrls.set(tcWithUrls + 1), Task.now(()))
            _ <- hasPicUrls.fold(CurrentStats.tweetCountWithPhotoUrls.set(tcWithPhotoUrls + 1), Task.now(()))

            // Pushing debug data to a queue and update all the rank signals
            _ <- CurrentStats.tweetData.enqueueOne(data)
            _ <- CurrentStats.topEmojis.set(emojiRank)
            _ <- CurrentStats.topHashtags.set(hashtagRank)
            _ <- CurrentStats.topUrlDomains.set(urlDomainRank)
          } yield ()
        )
      }
} 