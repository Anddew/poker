package com.anddew.poker.model

import enumeratum._


sealed abstract case class Rank private (symbol: Char, priority: Int) extends EnumEntry

object Rank extends Enum[Rank] {

  val ranks: Map[Char, Rank] = findValues.map(rank => (rank.symbol, rank)).toMap

  override def values: IndexedSeq[Rank] = findValues

  final case object Two extends Rank('2', 2)
  final case object Three extends Rank('3', 3)
  final case object Four extends Rank('4', 4)
  final case object Five extends Rank('5', 5)
  final case object Six extends Rank('6', 6)
  final case object Seven extends Rank('7', 7)
  final case object Eight extends Rank('8', 8)
  final case object Nine extends Rank('9', 9)
  final case object Ten extends Rank('T', 10)
  final case object Jack extends Rank('J', 11)
  final case object Queen extends Rank('Q', 12)
  final case object King extends Rank('K', 13)
  final case object Ace extends Rank('A', 14)

}
