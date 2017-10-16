package io.nary.condenser

import scala.io.Source._
import scalaz.concurrent.Task

import defs._
import Parsing._

object Emoji {
  import scalaz._
  import Scalaz._
  import argonaut._
  import Argonaut._

  implicit val EmojiDefinitionDecoder : DecodeJson[EmojiDef] = 
    DecodeJson(d =>
      for {
        n  <- (d --\ "short_name").as[String]
        t  <- (d --\ "text").as[Option[String]]
        tl <- (d --\ "texts").as[Option[List[String]]]
        u <- (d --\ "unified").as[Option[String]]
      } yield EmojiDef(n, tl.orNil ++ t.orNil , u.map(_.toUpperCase))
    )  
  
  def list : Task[List[EmojiDef]] = fromResource("/emoji_pretty.json")
    .flatMap(decode[List[EmojiDef]])

  def maps : Task[(Map[String, EmojiDef], Map[String, EmojiDef])] = list.map {l =>
    (l.foldLeft(Map.empty[String, EmojiDef]) { case (mo, e) =>
      mo ++ e.ascii.foldLeft(Map.empty[String, EmojiDef]) { case (mi, a) =>
        mi + (a -> e)
      }  
    },
    l.foldLeft(Map.empty[String, EmojiDef]) { case (mo, e) =>
      mo ++ e.unicodeHex.foldLeft(Map.empty[String, EmojiDef]) { case (mi, u) =>
        mi + (u -> e)
      }  
    }
    )
  }  

  def fromResource(path: String): Task[String] = Task.delay(
    fromInputStream(getClass.getResourceAsStream(path)).mkString
  )

  def getEmojiFrom(str: String)(asciiToEmojiDef: Map[String, EmojiDef], hexUnicodeToEmojiDef: Map[String, EmojiDef]) : List[EmojiDef] =
    asciiToEmojiDef.keys.foldLeft(List.empty[EmojiDef]) { case (l, e) =>
      if(str.contains(e))
        l :+ asciiToEmojiDef(e)
      else
        l  
    } ++
    str.toList.foldLeft(List.empty[EmojiDef]) { case (l, e) =>
      val hex = e.toHexString.toUpperCase
      
      if(hexUnicodeToEmojiDef.keys.toList.contains(hex))
        l :+ hexUnicodeToEmojiDef(hex)
      else
        l  
    }
  

}