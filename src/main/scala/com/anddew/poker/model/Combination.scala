package com.anddew.poker.model

sealed abstract class Combination(val weight: Int, val kickers: List[Rank])

object Combinations {

  case class StraightFlush(override val kickers: List[Rank]) extends Combination(9, kickers)
  case class FourOfAKind(override val kickers: List[Rank]) extends Combination(8, kickers)
  case class FullHouse(override val kickers: List[Rank]) extends Combination(7, kickers)
  case class Flush(override val kickers: List[Rank]) extends Combination(6, kickers)
  case class Straight(override val kickers: List[Rank]) extends Combination(5, kickers)
  case class ThreeOfAKind(override val kickers: List[Rank]) extends Combination(4, kickers)
  case class TwoPair(override val kickers: List[Rank]) extends Combination(3, kickers)
  case class Pair(override val kickers: List[Rank]) extends Combination(2, kickers)
  case class HighCard(override val kickers: List[Rank]) extends Combination(1, kickers)

}
