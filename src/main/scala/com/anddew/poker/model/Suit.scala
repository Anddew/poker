package com.anddew.poker.model

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
