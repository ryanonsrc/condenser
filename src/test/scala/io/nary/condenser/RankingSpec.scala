package io.nary.condenser

import org.scalatest.{FlatSpec, Matchers}

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
    val rankedPruned = ranked.prune
    rankedPruned.excessLoad should equal(0)

    val top3 = rankedPruned.top(3)
    // There should be no change here
    top3(0) should equal("Barclay")
    top3(1) should equal("Pulaski")
    top3(2) should equal("LaForge")
  }

}
