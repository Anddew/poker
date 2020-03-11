package com.anddew.poker.resolver

import cats.kernel.Order
import com.anddew.poker.model.Combination._
import com.anddew.poker.model.{Card, Combination, Rank, Suit}

import scala.collection.SortedSet


// TODO should be refactored to type class instances for each combination variant
trait Resolver {

  def resolve(cards: List[Card]): Combination

}

object Resolver {

  val STRAIGHT_SEQ = "AKQJT98765432A"
  val WHEEL_STRAIGHT_SEQ = "A5432"

  def apply(): Resolver = new Resolver() {
    override def resolve(cards: List[Card]): Combination = {
      import cats.implicits._

      cards.groupByNel(identity)(Order.fromOrdering).values.map(_.toList).toList match {
        case List(straightFlush @ _*) if {
          val isSameSuit = straightFlush.flatten.foldRight(Set.empty[Suit])((card, set) => set + card.suit).size == 1
          val ranks = straightFlush.flatten.foldRight(SortedSet.empty[Rank])((card, set) => set + card.rank)
          val isStraight = {
            val isAllDifferent = ranks.size == 5
            val pattern = ranks.mkString.r
            val isStraight = pattern.findFirstIn(STRAIGHT_SEQ).isDefined || pattern.findFirstIn(WHEEL_STRAIGHT_SEQ).isDefined
            isAllDifferent && isStraight
          }
          isSameSuit && isStraight
        }                                                                                                           => StraightFlush(straightFlush.flatten.head.rank)
        case List(four, kicker) if four.size == 4                                                                   => FourOfAKind(four.head.rank, kicker.head.rank)
        case List(three, pair) if three.size == 3 && pair.size == 2                                                 => FullHouse(three.head.rank, pair.head.rank)
        case List(flush @ _*) if flush.flatten.foldRight(Set.empty[Suit])((card, set) => set + card.suit).size == 1 => flush.flatten.map(_.rank) match {
          case Seq(k1, k2, k3, k4, k5) => Flush((k1, k2, k3, k4, k5))
        }
        case List(straight @ _*) if {
          val ranks = straight.flatten.foldRight(SortedSet.empty[Rank])((card, set) => set + card.rank)
          val isAllDifferent = ranks.size == 5
          val pattern = ranks.mkString.r
          val isStraight = pattern.findFirstIn(STRAIGHT_SEQ).isDefined || pattern.findFirstIn(WHEEL_STRAIGHT_SEQ).isDefined
          isAllDifferent && isStraight
        }                                                                                                           => Straight(straight.flatten.head.rank)
        case List(three, kicker1, kicker2) if three.size == 3                                                       => ThreeOfAKind(three.head.rank, (kicker1.head.rank, kicker2.head.rank))
        case List(pair1, pair2, kicker) if pair1.size == 2 && pair2.size == 2                                       => TwoPair(pair1.head.rank, pair2.head.rank, kicker.head.rank)
        case List(pair, kickers @ _*) if pair.size == 2                                                             => kickers.flatten.map(_.rank) match {
          case Seq(k1, k2, k3) => Pair(pair.head.rank, (k1, k2, k3))
        }
        case List(kickers @ _*)                                                                                     => kickers.flatten.map(_.rank) match {
          case Seq(k1, k2, k3, k4, k5) => HighCard(k1, k2, k3, k4, k5)
        }
      }

    }
  }

}
