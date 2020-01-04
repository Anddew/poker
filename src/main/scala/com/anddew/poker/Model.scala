package com.anddew.poker

import com.anddew.poker.Suits.{Spades, Suit}
import com.anddew.poker.Ranks.Rank

object Suits {

  sealed abstract class Suit(val symbol: Char)

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

object Ranks {

  sealed abstract class Rank(val symbol: Char, val priority: Int) {
    def of(suit: Suit): Card = Card(this, suit) // convert to CardOps
    override def toString: String = symbol.toString
  }

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

class Card(val rank: Rank, val suit: Suit) {

  def canEqual(a: Any) = a.isInstanceOf[Card]

  override def equals(that: Any): Boolean =
    that match {
      case that: Card =>
        that.canEqual(this) &&
          this.rank == that.rank
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    prime * result + rank.priority
  }

  override def toString: String = s"${ rank.symbol }${ suit.symbol }"
}

object Card {
  def apply(rank: Rank, suit: Suit): Card = new Card(rank, suit)
}


object Combinations {

  sealed abstract class Combination(val weight: Int, val cards: List[Card])

  case class StraightFlush(override val cards: List[Card]) extends Combination(9, cards)
  case class Four(override val cards: List[Card]) extends Combination(8, cards)
  case class FullHouse(override val cards: List[Card]) extends Combination(7, cards)
  case class Flush(override val cards: List[Card]) extends Combination(6, cards)
  case class Straight(override val cards: List[Card]) extends Combination(5, cards)
  case class Three(override val cards: List[Card]) extends Combination(4, cards)
  case class TwoPair(override val cards: List[Card]) extends Combination(3, cards)
  case class Pair(override val cards: List[Card]) extends Combination(2, cards)
  case class HighCard(override val cards: List[Card]) extends Combination(1, cards)

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
      .orElseBy(_.cards)

  }

}
