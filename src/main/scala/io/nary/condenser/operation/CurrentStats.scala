package io.nary.condenser

package operation

import scalaz.stream._
import scalaz.stream.async._
import scalaz.stream.async.mutable._

import defs._
import stats._

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