package io.nary.condenser

import argonaut._
import scalaz.concurrent.Task

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
}