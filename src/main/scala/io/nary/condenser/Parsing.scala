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

import argonaut._
import scalaz.concurrent.Task

import defs._

/**
 * Some Parsing apparatus to help us out.
 */
object Parsing {
  implicit class OptionStringOps(val optionString: Option[String]) extends AnyVal {
    def orNil : List[String] = optionString.map(s => List(s)) getOrElse Nil
  }
  
  implicit class OptionStringListOps(val optionListString: Option[List[String]]) extends AnyVal {
    def orNil : List[String] = optionListString getOrElse Nil
  }
  
  def decode[A : DecodeJson](json: String): Task[A] = Parse.decodeEither[A](json)
    .fold(d => Task.fail(new RuntimeException(s"On JSON: $json \n\n\n Message: $d")), Task.delay(_))

  def decodeOrElse[A : DecodeJson](json: String)(default: => A): Task[A] = Parse.decodeEither[A](json)
    .fold(_ => Task.delay(default), Task.delay(_)) 

  def extractDomain(url: TweetUrl) : String = url.url.split("//").flatMap(_.split("/")).tail.head

  def picUrl(tu: TweetUrl) : Boolean =
    (tu.url contains "pic.twitter.com") || (tu.url contains "instagram.com")
}