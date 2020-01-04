package com.anddew.poker

import scala.util.matching.Regex

trait Parser[A] {
  def parse(input: String): Either[String, List[A]]
}

class CardParser extends Parser[Card] {
  override def parse(input: String): Either[String, List[Card]] = {
    input.toSeq
      .sliding(2, 2)
      .map(pair => for {
        rank <- Ranks.parse(pair.head)
        suit <- Suits.parse(pair.last)
      } yield rank of suit
      )
      .foldRight(Right(Nil): Either[String, List[Card]])((ei, list) =>
        for {
          x <- ei.right
          y <- list.right
        } yield x :: y
      )
  }
}

class TexasHoldemParser {

  val INPUT_REGEX: Regex = """^(([23456789TJQKA][hdcs]){5})(( ([23456789TJQKA][hdcs]){2})+)$""".r("board", "", "hands")

  val parser = new CardParser()

  def parse(input: String): (List[Card], List[List[Card]]) = {
    INPUT_REGEX.findFirstMatchIn(input) match {
      case Some(submission) =>
        val boardString = submission.group("board")

        val board = parser.parse(boardString) match {
          case Right(list) => list
//          case Right(List(_*))                               => println("error. Expected 5 card on board.")
//          case Left(error)                                   => println(error)
        }

        val handsString = submission.group("hands").trim

        val cards = for {
          hand <- handsString.split("\\s+").toList
          cards = parser.parse(hand) match {
            case Right(hand) => hand
//            case Right(List(_*))          => println("error. Expected 2 card on hand.")
//            case Left(error)              => println(error)
          }
        } yield cards

        (board, cards)
  }
}




}
