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

import scala.io.Source._
import scalaz.concurrent.Task

import defs._
import Parsing._

/**
 * Here we load and process Emoji definition data for reference as we need to gather
 * statistics relating to Emojis detected within Tweets.
 */
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