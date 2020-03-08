package com.anddew.poker.model

import enumeratum.{Enum, EnumEntry}


sealed abstract case class Combination private (weight: Int)  extends EnumEntry

object Combination extends Enum[Combination] {

  override def values: IndexedSeq[Combination] = findValues

  // TODO smart constructor to resolve combination?
  final case object StraightFlush extends Combination(9)
  final case object FourOfAKind extends Combination(8)
  final case object FullHouse extends Combination(7)
  final case object Flush extends Combination(6)
  final case object Straight extends Combination(5)
  final case object ThreeOfAKind extends Combination(4)
  final case object TwoPair extends Combination(3)
  final case object Pair extends Combination(2)
  final case object HighCard extends Combination(1)

}
