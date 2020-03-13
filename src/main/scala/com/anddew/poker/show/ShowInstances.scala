package com.anddew.poker.show

import cats.Show
import cats.data.NonEmptyList
import com.anddew.poker.model.{Card, Hand, HandCombination, Rank, Suit}
import cats.implicits._
import com.anddew.poker.error.AppError


object ShowInstances {

  implicit val rankShow: Show[Rank] = rank => s"${ rank.symbol }"
  implicit val suitShow: Show[Suit] = suit => s"${ suit.symbol }"
  implicit val cardShow: Show[Card] = card => show"${ card.rank }${ card.suit }"
  implicit val cardListShow: Show[List[Card]] = cards => cards.iterator.map(_.show).mkString("")
  implicit val handShow: Show[Hand] = hand => show"${ hand.cards }"
  implicit val handCombinationShow: Show[HandCombination] = handCombination => show"${ handCombination.hand }"
  implicit val handCombinationListShow: Show[List[HandCombination]] = combinations =>
    combinations.groupByNel(identity)
      .view.mapValues(_.map(_.show).sorted.mkString_("="))
      .values.mkString(" ")

  implicit val errorShow: Show[AppError] = error => s"Error: $error"
  implicit val errorNelShow: Show[NonEmptyList[AppError]] = errors => errors.map(_.show).toList.mkString("<", ", ", ">")
  implicit def eitherShow[E, A](implicit errorShow: Show[E], valueShow: Show[A]): Show[Either[E, A]] = {
    case Left(left)   => errorShow.show(left)
    case Right(right) => valueShow.show(right)
  }

}
