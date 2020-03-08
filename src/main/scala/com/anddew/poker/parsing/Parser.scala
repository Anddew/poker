package com.anddew.poker.parsing

import cats.data.EitherT
import cats.effect.IO
import com.anddew.poker.model.Holdem.{OmahaHoldem, TexasHoldem}
import com.anddew.poker.model.{Card, Hand, Rank, Submission, Suit}
import com.anddew.poker.parsing.ParsingError.{EmptyInput, IllegalSymbol, InvalidInput}
import cats.syntax._
import cats.syntax.either._

trait Parser[A, B] {

  def parse(input: A): Either[ParsingError, B]

}

object Parser {

  def parse[A, B](input: A)(implicit parser: Parser[A, B]): Either[ParsingError, B] = parser.parse(input)

}

object ParserInstances {

  implicit val rankParser: Parser[Char, Rank] = symbol => Rank.ranks.get(symbol).toRight(IllegalSymbol(symbol))
  implicit val suitParser: Parser[Char, Suit] = symbol => Suit.suits.get(symbol).toRight(IllegalSymbol(symbol))

  implicit val cardParser: Parser[String, Card] = (input: String) => for {
    cardSymbols <- input match {
      case symbols if symbols.length == 2 => Right(symbols)
      case _                              => Left(InvalidInput(input))
    }
    rank <- Parser.parse[Char, Rank](cardSymbols(0))
    suit <- Parser.parse[Char, Suit](cardSymbols(1))
  } yield Card(rank, suit)

  implicit val cardListParser: Parser[String, List[Card]] = (input: String) => for {
    cardSymbols <- input.grouped(2).toList
    card <- Parser.parse[String, Card](cardSymbols)
  } yield card


  implicit val submissionParser: Parser[String, Submission] = (input: String) => {
    input.split(" ").toList match {
      case board :: hands => Right(Submission(board, hands.map(Hand.apply)))
      case Nil            => Left(EmptyInput)
    }

    //    TEXAS_HOLDEM_REGEX: Regex = """^(([23456789TJQKA][hdcs]){5})(( ([23456789TJQKA][hdcs]){2})+)$""".r("board", "", "hands")
    //    val OMAHA_HOLDEM_REGEX: Regex = """^(([23456789TJQKA][hdcs]){5})(( ([23456789TJQKA][hdcs]){4})+)$""".r("board", "", "hands")
    //
    //    val regex = holdem match {
    //      case TexasHoldem => TEXAS_HOLDEM_REGEX
    //      case OmahaHoldem => OMAHA_HOLDEM_REGEX
    //    }
    //
    //    regex.findFirstMatchIn(input) match {
    //      case Some(submission) =>
    //        val boardString = submission.group("board")
    //
    //        val board = cardParser.parse(boardString) match {
    //          case Right(list) => list
    //          //          case Right(List(_*))                               => println("error. Expected 5 card on board.")
    //          //          case Left(error)                                   => println(error)
    //        }
    //
    //        val handsString = submission.group("hands").trim
    //
    //        val hands = for {
    //          hand <- handsString.split("\\s+").toList
    //          cards = cardParser.parse(hand) match {
    //            case Right(hand) => hand
    //            //            case Right(List(_*))          => println("error. Expected 2 card on hand.")
    //            //            case Left(error)              => println(error)
    //          }
    //        } yield Hand(cards)
    //
    //        (board, hands)
    //        //      case None => println(s"Wrong input, cannot be parsed.")
    //
    //        input
    //          .toSeq
    //          .sliding(2, 2)
    //          .map(pair => for {
    //            rank <- Ranks.parse(pair.head)
    //            suit <- Suits.parse(pair.last)
    //          } yield rank of suit
    //          )
    //          .foldRight(Right(Nil): Either[String, List[Card]])((ei, list) =>
    //            for {
    //              x <- ei.right
    //              y <- list.right
    //            } yield x :: y
    //          )
    Left(InvalidInput(input)).withRight[Submission]
  }

}

sealed abstract class ParsingError(message: String) extends Throwable(message) {
  override def toString: String = message
}

object ParsingError {
  case object EmptyInput extends ParsingError("Input is empty.")
  case class InvalidInput(message: String) extends ParsingError(s"Invalid input: <$message>")
  case class IllegalSymbol(symbol: Char) extends ParsingError(s"Illegal symbol: <$symbol>")
}

