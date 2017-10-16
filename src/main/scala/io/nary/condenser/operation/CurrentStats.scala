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

import scalaz.stream._
import scalaz.stream.async._
import scalaz.stream.async.mutable._

import defs._
import stats._

/**
 * All data that is needed for collecting statistics is stored here, mostly as scalaz-stream Signals.
 */
object CurrentStats {
  import Ranking._
  
  val tweetStrings : Queue[String] = circularBuffer[String](100)
  val tweetData : Queue[TweetData] = circularBuffer[TweetData](100)
  val emojiDefinitionSize : Signal[(Int, Int)] = signalOf((0, 0))
  val tweetCount : Signal[Long] = signalOf(0L)
  val tweetVelocity : Signal[AverageVelocity] = signalOf(AverageVelocity(0.0, 0.0, 0.0))
  val topEmojis : Signal[Ranked[EmojiDef]] = signalOf(newRanking[EmojiDef](10))
  val topHashtags : Signal[Ranked[Hashtag]] = signalOf(newRanking[Hashtag](10))
  val topUrlDomains : Signal[Ranked[String]] = signalOf(newRanking[String](10))
  val tweetCountWithEmojis : Signal[Int] = signalOf(0)
  val tweetCountWithUrls : Signal[Int] = signalOf(0)
  val tweetCountWithPhotoUrls : Signal[Int] = signalOf(0)
}