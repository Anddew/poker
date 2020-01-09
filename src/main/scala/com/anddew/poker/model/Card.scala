package com.anddew.poker.model

case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"${ rank.symbol }${ suit.symbol }"
}
