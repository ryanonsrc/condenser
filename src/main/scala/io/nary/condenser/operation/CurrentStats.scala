package io.nary.condenser

package operation

import scalaz.stream._
import scalaz.stream.async._
import scalaz.stream.async.mutable._

import defs._
import stats._

object CurrentStats {
  val tweetStrings : Queue[String] = circularBuffer[String](100)
  val tweetData : Queue[TweetData] = circularBuffer[TweetData](100)
  val emojiDefinitionSize : Signal[(Int, Int)] = signalOf((0, 0))
  val tweetCount : Signal[Long] = signalOf(0L)
  val tweetVelocity : Signal[AverageVelocity] = signalOf(AverageVelocity(0.0, 0.0, 0.0))
}