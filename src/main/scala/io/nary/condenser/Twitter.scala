package io.nary.condenser

import defs._
import scala.language.higherKinds

import scalaz.stream.io.stdOutLines
import scalaz.concurrent.Task
import scalaz.stream.{Process, channel}

import scalaz._
import Scalaz._

import org.http4s.client.blaze._
import org.http4s.client.oauth1

import argonaut._
import Argonaut._
import Parsing._

object Twitter {

  type Tweets = Chunks[Process, Task, Json]

  def from(consumerKey: String, consumerSecret: String, accessToken: String, accessSecret: String) : Tweets = {
      import org.http4s.{Request, Method, Uri}
      import org.http4s.client.blaze.SimpleHttp1Client
      import jawnstreamz._

      implicit val argonautJawnFacade = jawn.support.argonaut.Parser.facade
      val httpClient = PooledHttp1Client()

      val consumer = oauth1.Consumer(consumerKey, consumerSecret)
      val token = oauth1.Token(accessToken, accessSecret)
      
      val request = Request(Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))

      for {
        signed <- Process.eval(oauth1.signRequest(request, consumer, 
          callback = None, verifier = None, token = Some(token)))
        streamed <- httpClient.streaming(signed)(_.body.parseJsonStream)
      } yield streamed
  }

  def picUrl(tu: TweetUrl) : Boolean =
    (tu.url contains "pic.twitter.com") || (tu.url contains "instagram.com")

  implicit val HashtagDecoder : DecodeJson[Hashtag] = 
    DecodeJson(h =>
      for {
        t <- (h --\ "text").as[String]
      } yield Hashtag(t)
    )

  implicit val TweetUrlDecoder : DecodeJson[TweetUrl] = 
    DecodeJson(u =>
      for {
        tu <- (u --\ "url").as[String]
      } yield TweetUrl(tu)
    )

  implicit val TweetDataDecoder : DecodeJson[TweetData] = 
    DecodeJson(d =>
      for {
        t  <- (d --\ "text").as[String]
        h  <- (d --\ "entities" --\ "hashtags").as[List[Hashtag]]
        u  <- (d --\ "entities" --\ "urls").as[List[TweetUrl]]
      } yield TweetData(t, 
        Nil,
        h,
        u.filterNot(picUrl), 
        u.filter(picUrl)
        ))
  
  def tweetData(json: Json)(asciiToEmojiDef: Map[String, EmojiDef], hexUnicodeToEmojiDef: Map[String, EmojiDef]) : Task[TweetData] = 
    decodeOrElse[TweetData](json.toString)(ignoredTweet).map(d => d.copy(
      emoji = Emoji.getEmojiFrom(d.text)(asciiToEmojiDef, hexUnicodeToEmojiDef)
    ))
}