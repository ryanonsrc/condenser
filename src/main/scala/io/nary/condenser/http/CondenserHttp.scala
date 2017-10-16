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

object CondenserHttp {
  val service = HttpService {
    case GET -> Root / "status" =>
      Ok("HTTP Service running.")
    
    case GET -> Root / "tweets" / "statistics" =>
      Ok(
        for {
          n <- CurrentStats.tweetCount.get
          v <- CurrentStats.tweetVelocity.get
          perHour = v.perHour
          perMin = v.perMinute
          perSec = v.perSecond
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
                  |        <b>$perSec</b> Tweets have been processed, per second (on average).
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
