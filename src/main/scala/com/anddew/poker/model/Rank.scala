package com.anddew.poker.model

import cats.Order
import enumeratum._


sealed abstract class Rank private (val symbol: Char, val weight: Int) extends EnumEntry

object Rank extends Enum[Rank] {

  implicit val rankOrdering: Ordering[Rank] = Ordering.by[Rank, Int](_.weight)
  implicit val rankOrder: Order[Rank] = Order.fromOrdering

  override def values: IndexedSeq[Rank] = findValues

  val ranks: Map[Char, Rank] = values.map(rank => (rank.symbol, rank)).toMap

  // TODO add config provided symbols from external configuration file
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
