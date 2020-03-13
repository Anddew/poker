package com.anddew.poker.model

import cats.Order


final case class Card(rank: Rank, suit: Suit)

object Card {

  implicit val cardOrdering: Ordering[Card] = Ordering.by[Card, Rank](_.rank).reverse
  implicit val cardOrder: Order[Card] = Order.fromOrdering

  implicit class CardSyntax(val rank: Rank) extends AnyVal {

    def of(suit: Suit): Card = Card(rank, suit)

  }

}



