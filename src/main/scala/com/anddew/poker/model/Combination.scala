package com.anddew.poker.model

import enumeratum.{Enum, EnumEntry}


sealed abstract class Combination private (val weight: Int) extends EnumEntry

object Combination extends Enum[Combination] {

  // TODO think...
  type Kickers = (Rank, Rank, Rank, Rank, Rank)
  type Kickers2 = (Rank, Rank)
  type Kickers3 = (Rank, Rank, Rank)

  override def values: IndexedSeq[Combination] = findValues

  final case class StraightFlush(kicker: Rank) extends Combination(9)
  final case class FourOfAKind(four: Rank, kicker: Rank) extends Combination(8)
  final case class FullHouse(three: Rank, pair: Rank) extends Combination(7)
  final case class Flush(kickers: Kickers) extends Combination(6)
  final case class Straight(kicker: Rank) extends Combination(5)
  final case class ThreeOfAKind(three: Rank, kickers: Kickers2) extends Combination(4)
  final case class TwoPair(pair1: Rank, pair2: Rank, kicker: Rank) extends Combination(3)
  final case class Pair(pair: Rank, kickers: Kickers3) extends Combination(2)
  final case class HighCard(kickers: Kickers) extends Combination(1)

}
