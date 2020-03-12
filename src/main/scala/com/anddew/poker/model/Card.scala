package com.anddew.poker.model

import cats.Order


final case class Card(rank: Rank, suit: Suit)

object Card {

  implicit val cardOrdering: Ordering[Card] = Ordering.by[Card, Rank](_.rank).reverse
  implicit val cardOrder: Order[Card] = Order.fromOrdering

}


final class CardSyntax(private val rank: Rank) extends AnyVal {

  def of(suit: Suit): Card = Card(rank, suit)

}
