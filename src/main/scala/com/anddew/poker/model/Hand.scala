package com.anddew.poker.model

case class Hand(cards: List[Card]) {
  override def toString: String = cards.mkString
}
