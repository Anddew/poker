package com.anddew.poker.model


final case class Card(rank: Rank, suit: Suit)


final class CardSyntax(private val rank: Rank) extends AnyVal {

  def of(suit: Suit): Card = Card(rank, suit)

}
