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
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}

import scala.concurrent.ExecutionContext


object Runner extends IOApp.WithContext with StrictLogging {

  override protected def executionContextResource: Resource[SyncIO, ExecutionContext] = {
    Resource.make(SyncIO(Executors.newCachedThreadPool()))(pool => SyncIO {
      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.SECONDS)
    }).map(ExecutionContext.fromExecutorService)
  }

  type SubmissionResult = EitherNel[AppError, List[HandCombination]]

  def resolveHand(board: Board, hand: Hand)(implicit holdem: Holdem): Combination = {
    import com.anddew.poker.show.ShowInstances.handShow
    logger.debug(s"resolve for ${Show[Hand].show(hand)} - ${Thread.currentThread().getName}")
    import Combination.combinationOrdering

    val combos = for {
      boardCards <- board.cards.combinations(holdem.boardSize - holdem.boardHoles)
      handCards <- hand.cards.combinations(holdem.handSize - holdem.handHoles)
    } yield (boardCards ::: handCards).combinations(5).map(Combination.findCombination).max

    combos.max
  }

  def processSubmission(submission: String)(implicit holdem: Holdem): IO[SubmissionResult] = {
    logger.debug(s"process - ${Thread.currentThread().getName}")
    import cats.instances.list._
    import cats.syntax.parallel._
    import cats.instances.either._
    import cats.syntax.traverse._

    val result = for {
      parsedSubmission <- Parser.parse(submission)
      validatedSubmission <- Validator.validate(parsedSubmission).toEither
    } yield validatedSubmission.hands
      .map(hand => IO(
        HandCombination(hand, resolveHand(validatedSubmission.board, hand))
      ))

    result.map(_.parSequence).sequence
  }

  def handleSubmission(implicit holdem: Holdem): IO[Unit] = for {
//    submission <- IO(StdIn.readLine)
    submission <- IO("3d3h5d8cAc 3c4h 9sJh 7cQh Ts5s TdQd Tc6c 2c4d 7d5c 7hKd")
    _ <- if (submission == null) IO.unit else for {
      result <- processSubmission(submission)
      _ <- IO(println(Show[SubmissionResult].show(result)))
//      _ <- handleSubmission
    } yield ()
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val holdem: Holdem = if (args.contains("--omaha")) OmahaHoldem else TexasHoldem

    for {
      _ <- handleSubmission
    } yield ExitCode.Success
  }

}
