package io.nary.condenser

import scalaz._
import Scalaz._

object Ranking {

  type Qty[A] = Map[A, Int]

  case class Ranked[A](maxContenders: Int, contenders: Set[A], items: Qty[A], cutoff: Int)

  def newRanking[A](maxContenders: Int) : Ranked[A] = Ranked[A](maxContenders, Set.empty[A], Map.empty, 0)

  implicit class QtyOps[A](val items: Qty[A]) extends AnyVal {
    def countOf(a: A) : Int = items.getOrElse(a, 0)
    def +(a: A) : Qty[A] = items + (a -> (countOf(a) + 1))
  }

  implicit class RankedOps[A](val ranked: Ranked[A]) extends AnyVal {
    implicit def orderingA : scala.math.Ordering[A] = new scala.math.Ordering[A] {
      def compare(x: A, y: A) : Int = 
        scala.math.Ordering.Int.compare(ranked.items(x), ranked.items(y))
    }
    
    def +(a: A) : Ranked[A] = include(ranked)(a)
    def lessContenders : Boolean = ranked.contenders.length < ranked.maxContenders
    def inContention(a: A) : Boolean = ranked.contenders.contains(a)

    def top(n: Int) : List[A] = 
      ranked.contenders.toList.sorted.reverse.slice(0, Math.min(n, ranked.maxContenders))

    def include(ranked: Ranked[A])(a: A) : Ranked[A] = (ranked.items + a) match {
      case newItems =>
        (ranked.lessContenders, ranked.inContention(a), newItems.countOf(a) > ranked.cutoff) match {
          case (true, false, false) =>  //  not all contenders, not yet in contention, not over cutoff
            ranked.copy(contenders = ranked.contenders + a, items = newItems)
          case (true, false, true) =>   // not all contenders, not yet in contention, over cutoff  
            ranked.copy(contenders = ranked.contenders + a, items = newItems, cutoff = newItems.countOf(a))
          case (true, true, false) =>  // not all contenders, already in contension, not over cutoff
            ranked.copy(items = newItems)
          case (true, true, true) =>  // not all contenders, already in contension, over cutoff
            ranked.copy(items = newItems, cutoff = newItems.countOf(a))
          case (false, false, false) => // all contenders, not yet in contension, not over cutoff
            ranked.copy(items = newItems)
          case (false, false, true)  => // all contenders, not yet in contension, over cutoff
            ranked.copy(contenders = (ranked.contenders + a).toList.sorted.tail.toSet, items = newItems)
          case (false, true, false)  => // all contenders, in contension, not over cutoff
            ranked.copy(items = newItems)
          case (false, true, true)   => // all contenders, in contension, over cutoff
            ranked.copy(items = newItems) 
        }
    }

    def excessLoad : Int = ranked.items.size - ranked.contenders.size
    def prune : Ranked[A] = ranked.copy(items = 
      ranked.contenders.foldLeft(Map.empty[A, Int]){ case (m, e) => 
        m + (e -> ranked.items(e))
      }
    )
    def pruneWhen(excessLoadThreadhold: Int) : Ranked[A] =
      if(excessLoad > excessLoadThreadhold) prune else ranked
  }
}