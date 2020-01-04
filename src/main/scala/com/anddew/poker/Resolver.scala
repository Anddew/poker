package com.anddew.poker

import com.anddew.poker.Combinations._
import com.anddew.poker.Ranks.Rank
import com.anddew.poker.Suits.Suit


class Resolver {

  import Combinations.Implicits._


  def resolve(board: List[Card], hand: List[Card]): Combination = {
    (board ++ hand)
      .combinations(5)
      .map(findTopCombination)
      .max
  }


  def findTopCombination(cards: List[Card]): Combination = {
    cards
      .groupBy(_.rank)
      .values
      .toList
      .sortWith((left, right) => {
        val sizeComparison = left.size.compare(right.size)
        if (sizeComparison != 0) sizeComparison > 0
        else (left.head.rank.priority - right.head.rank.priority) > 0
      })
    match {
      case List(straightFlush @ _*)
        if straightFlush.flatten.foldRight(Set.empty[Suit])(
          (card, set) => set + card.suit
        ).size == 1 &&
          straightFlush.flatten.foldRight(Set.empty[Rank])(
            (card, set) => set + card.rank
          ).toList
            .sorted
            .mkString.r
            .findFirstIn(Resolver.STRAIGHT_SEQ).isDefined                   => StraightFlush(straightFlush.flatten.toList)
      case List(four, kicker) if four.size == 4                             => Four(four ::: kicker)
      case List(three, two) if three.size == 3 && two.size == 2             => FullHouse(three ::: two)
      case List(flush @ _*)
        if flush.flatten.foldRight(Set.empty[Suit])(
          (card, set) => set + card.suit
        ).size == 1                                                         => Flush(flush.flatten.toList)
      case List(straight @ _*) if straight.flatten
        .foldRight(Set.empty[Rank])((card, set) => set + card.rank).toList
        .sorted
        .mkString.r
        .findFirstIn(Resolver.STRAIGHT_SEQ).isDefined                       => Straight(straight.flatten.toList)
      case List(three, kickers @ _*) if three.size == 3                     => Three(three ::: kickers.flatten.toList)
      case List(pair1, pair2, kicker) if pair1.size == 2 && pair2.size == 2 => TwoPair(pair1 ::: pair2 ::: kicker)
      case List(pair, kickers @ _*) if pair.size == 2                       => Pair(pair ::: kickers.flatten.toList)
      case List(kickers @ _*)                                               => HighCard(kickers.flatten.toList)
    }

  }


}

object Resolver {

  val STRAIGHT_SEQ = "AKQJT98765432A"

}
