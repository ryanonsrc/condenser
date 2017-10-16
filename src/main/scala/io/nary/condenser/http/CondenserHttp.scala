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

import org.http4s._
import org.http4s.dsl._
import org.http4s.MediaType
import org.http4s.headers.`Content-Type`
import org.http4s.server._

import scalaz.concurrent.Task

import scalaz._
import Scalaz._
import defs._

import operation.CurrentStats

/**
 *  All Service Endpoints for viewing statistics and observing internal state of the Condenser Engine (mainly for diagnostic purposes) 
 */
object CondenserHttp {
  val service = HttpService {
    case GET -> Root / "status" =>
      Ok("HTTP Service running.")
    
    case GET -> Root / "tweets" / "statistics" =>
      Ok(
        for {
          n <- CurrentStats.tweetCount.get
          v <- CurrentStats.tweetVelocity.get
          e <- CurrentStats.tweetCountWithEmojis.get
          u <- CurrentStats.tweetCountWithUrls.get
          p <- CurrentStats.tweetCountWithPhotoUrls.get
          te <- CurrentStats.topEmojis.get.map(_.top(10))
          th <- CurrentStats.topHashtags.get.map(_.top(10))
          td <- CurrentStats.topUrlDomains.get.map(_.top(10))
          perHour = v.perHour
          perMin = v.perMinute
          perSec = v.perSecond
          percentEmojis = (e.toDouble / n.toDouble) * 100
          percentUrls = (u.toDouble / n.toDouble) * 100
          percentPicUrls = (p.toDouble / n.toDouble) * 100
          tEmojis = te.map(_.name).mkString("( ", ", ", " )")
          tHashtags = th.map(_.text).mkString("( ", ", ", " )")
          tDomains = td.mkString("( ", ", ", " )")
        } yield s"""
                  | <html>
                  |      <head>
                  |        <title>Streaming Condenser -- Twitter Statistics</title>
                  |        <meta http-equiv="refresh" content="1"/>
                  |      </head>
                  |      <body>
                  |        <b>$n</b> Tweets have been processed, total.<br>
                  |        <b>$perHour</b> Tweets have been processed, per hour (on average).<br>
                  |        <b>$perMin</b> Tweets have been processed, per minute (on average).<br>
                  |        <b>$perSec</b> Tweets have been processed, per second (on average).<br>
                  |        <hr>
                  |        <strong>Top 10 Emojis: $tEmojis</strong><br> 
                  |        <strong>Top 10 Hashtags: $tHashtags</strong><br>
                  |        <strong>Top 10 Domains: $tDomains</strong><br>
                  |        <hr>
                  |        <strong>Percent of Tweets containing Emojis: $percentEmojis</strong><br> 
                  |        <strong>Percent of Tweets containing Urls: $percentUrls</strong><br>
                  |        <strong>Percent of Tweets containing Picture Urls: $percentPicUrls</strong><br>        
                  |      </body>
                  |    </html>
                """.stripMargin
      ).withContentType(Some(`Content-Type`(MediaType.`text/html`)))
      
    case GET -> Root / "debug" / "tweets" / n =>
      Ok{
        val tweets : String = int(n).flatMap{i => CurrentStats.tweetStrings.dequeue.take(i)
          .runFoldMap[Task, List[String]](s => Applicative[List].point(s))
          .map(l => l.reduce(_ + _))}.run

        tweets
      }.withContentType(Some(`Content-Type`(MediaType.`application/json`)))

    case GET -> Root / "debug" / "tweets" / "data" / n =>
      Ok{
        val td : List[TweetData] = int(n).flatMap{i => CurrentStats.tweetData.dequeue.take(i)
          .runFoldMap[Task, List[TweetData]](s => Applicative[List].point(s))}.run

        td.mkString("\n")
      }.withContentType(Some(`Content-Type`(MediaType.`application/json`)))

    case GET -> Root / "debug" / "emoji" / "info" =>
      Ok(CurrentStats.emojiDefinitionSize.get.map(e => s"ASCII Emoji Entries: ${e._1},  Unicode Emoji Entries ${e._2}"))
        .withContentType(Some(`Content-Type`(MediaType.`text/html`)))
  }

  def int(str: String) : Task[Int] = {
    str.parseInt.disjunction.toOption match {
      case Some(i) => Task.now(i)
      case None => Task.fail(new Exception("Invalid parameter."))
    }
  }
}
