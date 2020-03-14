package com.anddew.poker.parsing

import cats.data.NonEmptyList
import com.anddew.poker.error.ParsingError.{EmptyInput, IllegalSymbol, InvalidInput}
import com.anddew.poker.model.{Board, Card, Hand, Rank, Submission, Suit}

object ParserInstances {

  implicit val rankParser: Parser[Char, Rank] =
    symbol => Rank.ranks.get(symbol)
      .toRight(NonEmptyList.one(
        IllegalSymbol(s"Illegal symbol for Rank - $symbol."))
      )

  implicit val suitParser: Parser[Char, Suit] =
    symbol => Suit.suits.get(symbol)
      .toRight(NonEmptyList.one(
        IllegalSymbol(s"Illegal symbol for Suit - $symbol."))
      )

  implicit val cardParser: Parser[String, Card] = (input: String) => for {
    cardSymbols <- input match {
      case symbols if symbols.length == 2 => Right(symbols)
      case _                              => Left(NonEmptyList.one(InvalidInput(s"Card could be parsed of exactly 2 symbols. Current input: $input")))
    }
    rank <- Parser.parse[Char, Rank](cardSymbols(0))
    suit <- Parser.parse[Char, Suit](cardSymbols(1))
  } yield Card(rank, suit)


  implicit val submissionParser: Parser[String, Submission] = (input: String) => {
    import cats.instances.list.catsStdInstancesForList
    import cats.instances.either.catsStdInstancesForEither
    import cats.syntax.traverse._

    input.split(" ").toList
      .map(_.grouped(2).toList)
      .map(_.traverse(Parser.parse[String, Card](_))) match {
      case Nil            => Left(NonEmptyList.one(EmptyInput))
      case _ :: Nil       => Left(NonEmptyList.one(InvalidInput("No hands provided. There are should be at least one hand.")))
      case board :: hands => for {
        board <- board.map(Board)
        hands <- hands.map(_.map(Hand)).sequence
      } yield Submission(board, hands)
    }
  }

}
