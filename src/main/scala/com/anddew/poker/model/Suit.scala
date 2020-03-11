package com.anddew.poker.model

import enumeratum._


sealed abstract class Suit private (val symbol: Char) extends EnumEntry

object Suit extends Enum[Suit] {

  override def values: IndexedSeq[Suit] = findValues
  val suits: Map[Char, Suit] = findValues.map(suit => (suit.symbol, suit)).toMap

  final case object Hearts extends Suit('h')
  final case object Diamonds extends Suit('d')
  final case object Clubs extends Suit('c')
  final case object Spades extends Suit('s')

}
