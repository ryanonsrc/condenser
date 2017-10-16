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

import org.scalatest.{FlatSpec, Matchers}

/**
 * Given that the ranking logic is easily the most tricky logic to validate (even with the test endpoints, its hard to
 * get a sense if data is being ranked appropriately), we have a test here that given a list of names and frequencies
 * we shuffle them around and see that they are ranked as expected.
 */
class RankingSpec extends FlatSpec with Matchers {

  import Ranking._

  def rankAll[A](r: Ranked[A], items: List[A]) : Ranked[A] = 
    items.foldLeft(r) { case (ranked, a) => ranked + a }

  "ranking" should "allow us to choose top 10 ranking values" in {
    val names = List(
      ("Barclay", 150), ("Pulaski", 148), ("LaForge", 145), ("Q", 140), 
      ("Varley", 120), ("Troi", 110), ("O'Brien", 90), ("Picard", 85),
      ("Yar", 80), ("Crusher", 60),  ("Graves", 5), ("Soong", 30), ("Fajo", 8), 
      ("Kolrami", 49), ("Bok", 1), ("Quinn", 59), ("Riker", 46)
    )

    val randomOccurrences = 
      scala.util.Random.shuffle(
        names.flatMap { case (n, count) => List.fill(count)(n) }
      )

    val ranked = rankAll(newRanking[String](10), randomOccurrences)
    val top10 = ranked.top(10)

    top10.size should equal(10)
    top10(0) should equal("Barclay")
    top10(1) should equal("Pulaski")
    top10(2) should equal("LaForge")
    top10(3) should equal("Q")
    top10(4) should equal("Varley")
    top10(5) should equal("Troi")
    top10(6) should equal("O'Brien")
    top10(7) should equal("Picard")
    top10(8) should equal("Yar")
    top10(9) should equal("Crusher")

    ranked.excessLoad should equal(7)
    // Here we prove that pruning zeros out the excess load and preserves the top 10 contenders.
    val rankedPruned = ranked.prune
    rankedPruned.excessLoad should equal(0)
    val top10pruned = rankedPruned.top(10)
    // There should be no change here
    top10pruned(0) should equal("Barclay")
    top10pruned(1) should equal("Pulaski")
    top10pruned(2) should equal("LaForge")
    top10pruned(3) should equal("Q")
    top10pruned(4) should equal("Varley")
    top10pruned(5) should equal("Troi")
    top10pruned(6) should equal("O'Brien")
    top10pruned(7) should equal("Picard")
    top10pruned(8) should equal("Yar")
    top10pruned(9) should equal("Crusher")
  }

}
