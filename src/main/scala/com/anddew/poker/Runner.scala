package com.anddew.poker

import cats.Show
import cats.data.Validated.{Invalid, Valid}
import cats.data.{EitherNel, NonEmptyList, ValidatedNel}
import cats.effect.{ExitCode, IO, IOApp}
import com.anddew.poker.model.{Board, Combination, Hand, HandCombination, Holdem, Submission}

import scala.io.StdIn
import com.anddew.poker.error.AppError
import com.anddew.poker.error.ValidationError.IllegalLength
import com.anddew.poker.model.Holdem.{OmahaHoldem, TexasHoldem}
import com.anddew.poker.parsing.Parser
import com.anddew.poker.parsing.ParserInstances.submissionParser
import com.anddew.poker.resolver.Resolver
import com.anddew.poker.show.ShowInstances.{errorNelShow, handCombinationListShow}


object Runner extends IOApp {

  val OMAHA_HOLDEM_OPTION = "--omaha"

  val resolver = Resolver()

  // TODO add Validated validation
  def validate(submission: Submission)(implicit holdem: Holdem): ValidatedNel[AppError, Submission] =
    submission match {
      case submission @ Submission(board, hands) if {
        board.cards.size == holdem.boardSize &&
          hands.forall(_.cards.size == holdem.handSize)
      }      => Valid(submission)
      case _ => Invalid(NonEmptyList.one(IllegalLength(
        s"""Board size should be ${ holdem.boardSize },
            actual ${ submission.board.cards.size }
            and hands size should be ${ holdem.handSize },
            actuals ${ submission.hands.map(_.cards.size).mkString("<", ", ", ">") }
        """"
      )))
    }

  def resolveHand(board: Board, hand: Hand)(implicit holdem: Holdem): Combination = {
    val combos = for {
      boardCards <- board.cards.combinations(board.cards.size - holdem.boardHoles)
      handCards <- hand.cards.combinations(hand.cards.size - holdem.handHoles)
    } yield (boardCards ::: handCards).combinations(5).map(resolver.resolve).max

    combos.max
  }


  def processSubmission(submission: String)(implicit holdem: Holdem): EitherNel[AppError, List[HandCombination]] = for {
    parsedSubmission <- Parser.parse(submission)
    validatedSubmission <- validate(parsedSubmission).toEither
    handCombination = validatedSubmission.hands.map(hand => HandCombination(hand, resolveHand(validatedSubmission.board, hand)))
  } yield handCombination

  // TODO does not handle EOF
  def handleSubmission(implicit holdem: Holdem): IO[Unit] = for {
    submission <- IO(StdIn.readLine)
    _ <- if (submission != null) for {
      result <- IO(processSubmission(submission).fold(
        error => Show[NonEmptyList[AppError]].show(error),
        result => Show[List[HandCombination]].show(result)
      ))
      _ <- IO(println(result))
      _ <- handleSubmission
    } yield ()
    else IO.unit
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val holdem: Holdem = if (args.contains(OMAHA_HOLDEM_OPTION)) OmahaHoldem else TexasHoldem

    for {
      _ <- handleSubmission
    } yield ExitCode.Success
  }

}
