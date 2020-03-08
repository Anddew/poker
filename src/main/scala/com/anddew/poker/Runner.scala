package com.anddew.poker

import cats.data.ValidatedNel
import cats.effect.{ExitCode, IO, IOApp}
import com.anddew.poker.model.{Board, Card, Combination, Hand, HandCombination, Holdem, Submission}

import scala.io.StdIn
import cats.syntax.all._
import com.anddew.poker.model.Holdem.{OmahaHoldem, TexasHoldem}
import com.anddew.poker.parsing.Parser
import com.anddew.poker.parsing.ParserInstances.submissionParser
import com.anddew.poker.validation.ValidationError


object Runner extends IOApp {

  val OMAHA_HOLDEM_OPTION = "--omaha"


  def validate(submission: Submission)(implicit holdem: Holdem): ValidatedNel[ValidationError, Submission] = ???


  def resolveHand(board: Board, hand: Hand)(implicit holdem: Holdem): Combination = {
    val combos = for {
      boardCards <- board.cards.combinations(board.cards.size - holdem.boardHoles)
      handCards <- hand.cards.combinations(hand.cards.size - holdem.handHoles)
      combo = (boardCards ::: handCards).combinations(5).map(findTopCombination).max
    } yield combo

    combos.max
  }


  def processSubmission(submission: String)(implicit holdem: Holdem): Either[String, List[HandCombination]] =
    for {
      parsed <- Parser.parse(submission)
      validated <- validate(parsed)
      handCombination = validated.hands.map(hand => HandCombination(hand, resolveHand(validated.board, hand)))
    } yield handCombination


  def handleSubmission(implicit holdem: Holdem): IO[Unit] = {
    for {
      submission <- IO(StdIn.readLine)
      _ <- if (submission == null)
        IO.unit
      else {
        processSubmission(submission).fold(
          error => println(show"$error"),
          result => println(show"$result")
          // TODO Show[List[HandCombination]] to print by "=" is equals or by " " for ordering strength
        )
        handleSubmission
      }
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val holdem: Holdem = if (args.contains(OMAHA_HOLDEM_OPTION)) OmahaHoldem else TexasHoldem

    for {
      _ <- handleSubmission
    } yield ExitCode.Success
  }


  def findTopCombination(cards: List[Card]): Combination = {

    //    val STRAIGHT_SEQ = "AKQJT98765432A"
    //    val WHEEL_STRAIGHT_SEQ = "A5432"
    //
    //    cards
    //      .groupBy(_.rank)
    //      .values
    //      .toList
    //      .sortWith((left, right) => {
    //        val sizeComparison = left.size.compare(right.size)
    //        if (sizeComparison != 0) sizeComparison > 0
    //        else (left.head.rank.priority - right.head.rank.priority) > 0
    //      })
    //    match {
    //      case List(straightFlush @ _*)
    //        if {
    //          val suits = straightFlush.flatten.foldRight(Set.empty[Suit])(
    //            (card, set) => set + card.suit
    //          )
    //          val ranks = straightFlush.flatten.foldRight(Set.empty[Rank])(
    //            (card, set) => set + card.rank
    //          )
    //          val pattern = ranks
    //            .toList
    //            .sorted(Ordering[Rank].reverse)
    //            .mkString.r
    //          suits.size == 1 && ranks.size == 5 && (
    //            pattern.findFirstIn(STRAIGHT_SEQ).isDefined ||
    //              pattern.findFirstIn(WHEEL_STRAIGHT_SEQ).isDefined)
    //        } => {
    //        val ranks = straightFlush.flatten.map(_.rank).toList
    //        if (ranks.head == Ace && ranks.tail.head == Five)
    //          StraightFlush(ranks.tail ::: (ranks.head :: Nil))
    //        else {
    //          StraightFlush(ranks)
    //        }
    //      }
    //      case List(four, kicker) if four.size == 4 => FourOfAKind(four.map(_.rank) ::: kicker.map(_.rank))
    //      case List(three, two) if three.size == 3 && two.size == 2 => FullHouse(three.map(_.rank) ::: two.map(_.rank))
    //      case List(flush@_*)
    //        if flush.flatten.foldRight(Set.empty[Suit])(
    //          (card, set) => set + card.suit
    //        ).size == 1 => Flush(flush.flatten.map(_.rank).toList)
    //      case List(straight@_*)
    //        if {
    //          val ranks = straight.flatten.foldRight(Set.empty[Rank])((
    //            card, set) => set + card.rank
    //          )
    //          val pattern = ranks
    //            .toList
    //            .sorted(Ordering[Rank].reverse)
    //            .mkString.r
    //          ranks.size == 5 && (
    //            pattern.findFirstIn(Resolver.STRAIGHT_SEQ).isDefined ||
    //              pattern.findFirstIn(Resolver.WHEEL_STRAIGHT_SEQ).isDefined)
    //        } => {
    //        val ranks = straight.flatten.map(_.rank).toList
    //        if (ranks.head == Ace && ranks.tail.head == Five)
    //          Straight(ranks.tail ::: (ranks.head :: Nil))
    //        else {
    //          Straight(ranks)
    //        }
    //      }
    //      case List(three, kickers@_*) if three.size == 3 => ThreeOfAKind(three.map(_.rank) ::: kickers.flatten.map(_.rank).toList)
    //      case List(pair1, pair2, kicker) if pair1.size == 2 && pair2.size == 2 => TwoPair(pair1.map(_.rank) ::: pair2.map(_.rank) ::: kicker.map(_.rank))
    //      case List(pair, kickers@_*) if pair.size == 2 => Pair(pair.map(_.rank) ::: kickers.flatten.map(_.rank).toList)
    //      case List(kickers@_*) => HighCard(kickers.flatten.map(_.rank).toList)
    //    }

    ???
  }

}
