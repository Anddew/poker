package com.anddew.poker

import java.util.concurrent.{Executors, TimeUnit}

import cats.Show
import cats.data.EitherNel
import cats.effect.{ExitCode, IO, IOApp, Resource, SyncIO}
import com.anddew.poker.model.{Board, Combination, Hand, HandCombination, Holdem}

import scala.io.StdIn
import com.anddew.poker.error.AppError
import com.anddew.poker.model.Holdem.{OmahaHoldem, TexasHoldem}
import com.anddew.poker.parsing.Parser
import com.anddew.poker.parsing.ParserInstances.submissionParser
import com.anddew.poker.validation.ValidatorInstances.submissionValidator
import com.anddew.poker.show.ShowInstances.{eitherShow, errorNelShow, handCombinationListShow}
import com.anddew.poker.validation.Validator

import scala.concurrent.ExecutionContext


object Runner extends IOApp.WithContext {

  override protected def executionContextResource: Resource[SyncIO, ExecutionContext] = {
    Resource.make(SyncIO(
      Executors.newCachedThreadPool())
    )(pool => SyncIO {
      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.SECONDS)
    }).map(ExecutionContext.fromExecutorService)
  }

  type SubmissionResult = EitherNel[AppError, List[HandCombination]]

  def resolveHand(board: Board, hand: Hand)(implicit holdem: Holdem): HandCombination = {
    import Combination.combinationOrdering

    val combinations = for {
      boardCards <- board.cards.combinations(holdem.boardSize - holdem.boardHoles)
      handCards <- hand.cards.combinations(holdem.handSize - holdem.handHoles)
      combination <- (boardCards ::: handCards).combinations(5)
    } yield Combination.findCombination(combination)

    HandCombination(hand, combinations.max)
  }

  def processSubmission(submission: String)(implicit holdem: Holdem): IO[SubmissionResult] = {
    import cats.instances.list._
    import cats.syntax.parallel._
    import cats.instances.either._
    import cats.syntax.traverse._

    val result = for {
      parsedSubmission <- Parser.parse(submission)
      validatedSubmission <- Validator.validate(parsedSubmission).toEither
    } yield validatedSubmission.hands.map(hand => IO(
      resolveHand(validatedSubmission.board, hand)
    )).parSequence

    result.sequence
  }

  def handleSubmission(implicit holdem: Holdem): IO[Unit] = for {
    submission <- IO(StdIn.readLine)
    _ <- if (submission == null) IO.unit else for {
      result <- processSubmission(submission)
      _ <- IO(println(Show[SubmissionResult].show(result)))
      _ <- handleSubmission
    } yield ()
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val holdem: Holdem = if (args.contains("--omaha")) OmahaHoldem else TexasHoldem

    for {
      _ <- handleSubmission
    } yield ExitCode.Success
  }

}
