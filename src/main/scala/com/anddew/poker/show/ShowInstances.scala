package com.anddew.poker.show

import cats.Show
import com.anddew.poker.model.{Card, Hand, Rank, Suit}
import cats.implicits.showInterpolator
import cats.syntax._

object ShowInstances {

  import ListShowInstances._

  implicit val rankShow: Show[Rank] = rank => s"${ rank.symbol }"
  implicit val suitShow: Show[Suit] = suit => s"${ suit.symbol }"
  implicit val cardShow: Show[Card] = card => show"${ card.rank }${ card.suit }"
  implicit val handShow: Show[Hand] = hand => show"${ hand.cards }"
  implicit val cardListShow: Show[List[Card]] = listWithoutSeparatorShow
  implicit val handListShow: Show[List[Hand]] = listWithEqualsSeparatorShow

  object ListShowInstances {

    implicit def listWithEqualsSeparatorShow[A](implicit show: Show[A]): Show[List[A]] =
      listWithSeparatorShow[A]("=")

    implicit def listWithWhitespaceSeparatorShow[A](implicit show: Show[A]): Show[List[A]] =
      listWithSeparatorShow[A](" ")

    implicit def listWithoutSeparatorShow[A](implicit show: Show[A]): Show[List[A]] =
      listWithSeparatorShow[A]("")

    private[this] def listWithSeparatorShow[A](separator: String)(implicit show: Show[A]): Show[List[A]] =
      _.iterator.map(show.show).mkString(separator)

  }




}
