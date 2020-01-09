package com.anddew.poker.model

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
