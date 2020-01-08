package com.anddew.poker

sealed abstract class Holdem(val boardHoles: Int, val handHoles: Int)
case object TexasHoldem extends Holdem(0, 0)
case object OmahaHoldem extends Holdem(2, 2)

sealed abstract class Suit(val symbol: Char)

object Suits {

  case object Hearts extends Suit('h')
  case object Diamonds extends Suit('d')
  case object Clubs extends Suit('c')
  case object Spades extends Suit('s')

  def parse(symbol: Char): Either[String, Suit] = symbol match {
    case Hearts.symbol   => Right(Hearts)
    case Diamonds.symbol => Right(Diamonds)
    case Clubs.symbol    => Right(Clubs)
    case Spades.symbol   => Right(Spades)
    case symbol          => Left(s"Unknown symbol '$symbol' for suits.")
  }

}

sealed abstract class Rank(val symbol: Char, val priority: Int) {
  def of(suit: Suit): Card = Card(this, suit) // convert to CardOps
  override def toString: String = symbol.toString
}

object Ranks {

  case object Two extends Rank('2', 2)
  case object Three extends Rank('3', 3)
  case object Four extends Rank('4', 4)
  case object Five extends Rank('5', 5)
  case object Six extends Rank('6', 6)
  case object Seven extends Rank('7', 7)
  case object Eight extends Rank('8', 8)
  case object Nine extends Rank('9', 9)
  case object Ten extends Rank('T', 10)
  case object Jack extends Rank('J', 11)
  case object Queen extends Rank('Q', 12)
  case object King extends Rank('K', 13)
  case object Ace extends Rank('A', 14)

  def parse(symbol: Char): Either[String, Rank] = symbol match {
    case Two.symbol   => Right(Two)
    case Three.symbol => Right(Three)
    case Four.symbol  => Right(Four)
    case Five.symbol  => Right(Five)
    case Six.symbol   => Right(Six)
    case Seven.symbol => Right(Seven)
    case Eight.symbol => Right(Eight)
    case Nine.symbol  => Right(Nine)
    case Ten.symbol   => Right(Ten)
    case Jack.symbol  => Right(Jack)
    case Queen.symbol => Right(Queen)
    case King.symbol  => Right(King)
    case Ace.symbol   => Right(Ace)
    case symbol       => Left(s"Unknown symbol '$symbol' for rank.")
  }

}

case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"${ rank.symbol }${ suit.symbol }"
}

case class Hand(cards: List[Card]) {
  override def toString: String = cards.mkString
}

case class HandCombination(hand: Hand, combination: Combination)

sealed abstract class Combination(val weight: Int, val kickers: List[Rank])

object Combinations {

  case class StraightFlush(override val kickers: List[Rank]) extends Combination(9, kickers)
  case class Four(override val kickers: List[Rank]) extends Combination(8, kickers)
  case class FullHouse(override val kickers: List[Rank]) extends Combination(7, kickers)
  case class Flush(override val kickers: List[Rank]) extends Combination(6, kickers)
  case class Straight(override val kickers: List[Rank]) extends Combination(5, kickers)
  case class Three(override val kickers: List[Rank]) extends Combination(4, kickers)
  case class TwoPair(override val kickers: List[Rank]) extends Combination(3, kickers)
  case class Pair(override val kickers: List[Rank]) extends Combination(2, kickers)
  case class HighCard(override val kickers: List[Rank]) extends Combination(1, kickers)

  object Implicits {

    implicit def rankOrdering: Ordering[Rank] = Ordering.by(_.priority)

    implicit def cardOrdering: Ordering[Card] = Ordering.by(_.rank)

    implicit def listOrdering[A](implicit ord: Ordering[A]): Ordering[List[A]] =
      (left: List[A], right: List[A]) => {
        if (left.isEmpty) 0 else {
          val result = Ordering[A].compare(left.head, right.head)
          if (result != 0) result else Ordering[List[A]].compare(left.tail, right.tail)
        }
      }

    implicit def combinationOrdering: Ordering[Combination] = Ordering
      .by[Combination, Int](_.weight)
      .orElseBy(_.kickers)

  }

}
