package com.anddew.poker.validation

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.anddew.poker.error.{AppError, ValidationError}
import com.anddew.poker.error.ValidationError.IllegalLength
import com.anddew.poker.model.{Holdem, Submission}

object ValidatorInstances {

  implicit def submissionValidator(implicit holdem: Holdem): Validator[Submission] = {
    case submission @ Submission(board, hands) if board.cards.size == holdem.boardSize &&
      hands.forall(_.cards.size == holdem.handSize) => Valid[Submission](submission)
    case submission                                 => Invalid[NonEmptyList[AppError]](NonEmptyList.one(
      IllegalLength(
        s"""Board size should be ${ holdem.boardSize },
            actual ${ submission.board.cards.size }
            and hands size should be ${ holdem.handSize },
            actuals ${ submission.hands.map(_.cards.size).mkString("<", ", ", ">") }
        """"
      ))).asInstanceOf[ValidatedNel[ValidationError, Submission]]
  }

}
