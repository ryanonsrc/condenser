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

import scalaz._
import Scalaz._

/**
 * Performant Top-N computations on an infinite stream actually requires a heuristic of sorts (since we can obviously
 * only provide Top-N rankings for data we have seen -- thus far).  And we need to strike a balance between collecting
 * all the ranking information, since we started the condenser, and only keeping data that is recent enough to be actually
 * relevant.  The approach we use here makes an attempt to achieve that balance.  While it keeps a large hashtable of all
 * the count-based rankings, it also maintains a list of "contenders". Ideally, we may want that list to be substantially larger
 * than whatever value of N, we would want to perform a "top" operation, but that's not absolutely necessary (in fact, we 
 * are simply using 10 contenders and look at all of them for the "top" values).  Once the map of counts gets too large,
 * we can wipe out the map, while preserving the list of contenders and subsequently repopulating the map with only the counts
 * from the contenders. The large the list of contenders, the less likely you will lose important high-ranking values, but if the
 * list is too long: you have a longer list to sort, process, etc for performing "Top" operations, and ranking updates.  As usual
 * its a matter of choosing the right balance for what you are ranking.
 */
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

    /**
     * This is the core ranking logic that drives the interplay between items appearing
     * and dissapearing from the list of contenders.
     */
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

    // A large value here indicates that we have a ton of counts, statistically: most being too small
    // to ever reach any top-N list.
    def excessLoad : Int = ranked.items.size - ranked.contenders.size
    
    // Here we "prune" by clearing the map, but repopulating it based on the contender counts.
    // After a prune occurs, the rankings will appear "frozen" for a bit, until other high ranking values
    // surpass them.  Indeed, we may consider to have a mechanism of "re-evaluating" contenders and swapping them
    // out with other high ranking values.
    def prune : Ranked[A] = ranked.copy(items = 
      ranked.contenders.foldLeft(Map.empty[A, Int]){ case (m, e) => 
        m + (e -> ranked.items(e))
      }
    )

    def pruneWhen(excessLoadThreadhold: Int) : Ranked[A] =
      if(excessLoad > excessLoadThreadhold) prune else ranked
  }
}