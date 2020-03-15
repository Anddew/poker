package com.anddew.poker.model

import cats.Show
import cats.implicits._
import com.anddew.poker.model.Rank._

import scala.collection.{Set, SortedSet}


sealed abstract class Combination private(val weight: Int)

object Combination {

  type CardsToCombination = List[Card] => Option[Combination]

  implicit val combinationOrdering: Ordering[Combination] = (lc: Combination, rc: Combination) => (lc, rc) match {
    case (ls: StraightFlush, rs: StraightFlush) => Ordering[StraightFlush].compare(ls, rs)
    case (lf: FourOfAKind, rf: FourOfAKind)     => Ordering[FourOfAKind].compare(lf, rf)
    case (lf: FullHouse, rf: FullHouse)         => Ordering[FullHouse].compare(lf, rf)
    case (lf: Flush, rf: Flush)                 => Ordering[Flush].compare(lf, rf)
    case (ls: Straight, rs: Straight)           => Ordering[Straight].compare(ls, rs)
    case (lt: ThreeOfAKind, rt: ThreeOfAKind)   => Ordering[ThreeOfAKind].compare(lt, rt)
    case (lt: TwoPair, rt: TwoPair)             => Ordering[TwoPair].compare(lt, rt)
    case (lp: Pair, rp: Pair)                   => Ordering[Pair].compare(lp, rp)
    case (lh: HighCard, rh: HighCard)           => Ordering[HighCard].compare(lh, rh)
    case _                                      => Ordering[Int].compare(lc.weight, rc.weight)
  }


  def findCombination(cards: List[Card]): Combination = {
    import com.anddew.poker.show.ShowInstances.cardListShow
    println(s"find for ${Show[List[Card]].show(cards)} - ${Thread.currentThread().getName}")
    checkAll.iterator
      .map(_.apply(cards))
      .find(_.isDefined)
      .flatten
      .getOrElse(sys.error(s"Cannot resolve combination for cards $cards.")) // should never happens
  }

  private val checkAll: List[CardsToCombination] = List(
    StraightFlush.of,
    FourOfAKind.of,
    FullHouse.of,
    Flush.of,
    Straight.of,
    ThreeOfAKind.of,
    TwoPair.of,
    Pair.of,
    HighCard.of
  )


  sealed abstract case class StraightFlush private(kicker: Rank) extends Combination(9)

  object StraightFlush {

    implicit val straightFlushOrdering: Ordering[StraightFlush] = Ordering.by(_.kicker)

    private[model] def of(cards: List[Card]): Option[StraightFlush] = for {
      _ <- sameSuit(cards)
      kicker <- isStraight(cards)
    } yield new StraightFlush(kicker) {}
  }


  sealed abstract case class FourOfAKind private(four: Rank, kicker: Rank) extends Combination(8)

  object FourOfAKind {

    implicit val fourOfAKindOrdering: Ordering[FourOfAKind] = Ordering
      .by[FourOfAKind, Rank](_.four)
      .orElseBy[Rank](_.kicker)

    private[model] def of(cards: List[Card]): Option[FourOfAKind] = {
      combineCards(cards) match {
        case List(List(fourKicker, _, _, _), List(kicker)) => new FourOfAKind(fourKicker.rank, kicker.rank) {}.some
        case _                                             => None
      }
    }
  }


  sealed abstract case class FullHouse private(three: Rank, pair: Rank) extends Combination(7)

  object FullHouse {

    implicit val fullHouseOrdering: Ordering[FullHouse] = Ordering
      .by[FullHouse, Rank](_.three)
      .orElseBy[Rank](_.pair)

    private[model] def of(cards: List[Card]): Option[FullHouse] = {
      combineCards(cards) match {
        case List(List(threeKicker, _, _), List(pairKicker, _)) => new FullHouse(threeKicker.rank, pairKicker.rank) {}.some
        case _                                                  => None
      }
    }
  }


  sealed abstract case class Flush private(k1: Rank, k2: Rank, k3: Rank, k4: Rank, k5: Rank) extends Combination(6)

  object Flush {

    implicit val flushOrdering: Ordering[Flush] = Ordering
      .by[Flush, Rank](_.k1)
      .orElseBy[Rank](_.k2)
      .orElseBy[Rank](_.k3)
      .orElseBy[Rank](_.k4)
      .orElseBy[Rank](_.k5)

    private[model] def of(cards: List[Card]): Option[Flush] = {
      cards.sorted match {
        case cards @ List(k1, k2, k3, k4, k5) if sameSuit(cards).isDefined => new Flush(k1.rank, k2.rank, k3.rank, k4.rank, k5.rank) {}.some
        case _                                                             => None
      }
    }
  }


  sealed abstract case class Straight private(kicker: Rank) extends Combination(5)

  object Straight {

    implicit val straightOrdering: Ordering[Straight] = Ordering.by[Straight, Rank](_.kicker)

    private[model] def of(cards: List[Card]): Option[Straight] = for {
      kicker <- isStraight(cards)
    } yield new Straight(kicker) {}
  }


  sealed abstract case class ThreeOfAKind private(three: Rank, k1: Rank, k2: Rank) extends Combination(4)

  object ThreeOfAKind {

    implicit val threeOfAKindOrdering: Ordering[ThreeOfAKind] = Ordering
      .by[ThreeOfAKind, Rank](_.three)
      .orElseBy[Rank](_.k1)
      .orElseBy[Rank](_.k2)

    private[model] def of(cards: List[Card]): Option[ThreeOfAKind] = {
      combineCards(cards) match {
        case List(List(threeKicker, _, _), List(k1), List(k2)) => new ThreeOfAKind(threeKicker.rank, k1.rank, k2.rank) {}.some
        case _                                                 => None
      }
    }
  }


  sealed abstract case class TwoPair private(pair1: Rank, pair2: Rank, kicker: Rank) extends Combination(3)

  object TwoPair {

    implicit val twoPairOrdering: Ordering[TwoPair] = Ordering
      .by[TwoPair, Rank](_.pair1)
      .orElseBy[Rank](_.pair2)
      .orElseBy[Rank](_.kicker)

    private[model] def of(cards: List[Card]): Option[TwoPair] = {
      combineCards(cards) match {
        case List(List(pair1Kicker, _), List(pair2Kicker, _), List(kicker)) => new TwoPair(pair1Kicker.rank, pair2Kicker.rank, kicker.rank) {}.some
        case _                                                              => None
      }
    }
  }


  sealed abstract case class Pair private(pair: Rank, k1: Rank, k2: Rank, k3: Rank) extends Combination(2)

  object Pair {

    implicit val pairOrdering: Ordering[Pair] = Ordering
      .by[Pair, Rank](_.pair)
      .orElseBy[Rank](_.k1)
      .orElseBy[Rank](_.k2)
      .orElseBy[Rank](_.k3)

    private[model] def of(cards: List[Card]): Option[Pair] = {
      combineCards(cards) match {
        case List(List(pairKicker, _), List(k1), List(k2), List(k3)) => new Pair(pairKicker.rank, k1.rank, k2.rank, k3.rank) {}.some
        case _                                                       => None
      }
    }
  }


  sealed abstract case class HighCard private(k1: Rank, k2: Rank, k3: Rank, k4: Rank, k5: Rank) extends Combination(1)

  object HighCard {

    implicit val highCardOrdering: Ordering[HighCard] = Ordering
      .by[HighCard, Rank](_.k1)
      .orElseBy[Rank](_.k2)
      .orElseBy[Rank](_.k3)
      .orElseBy[Rank](_.k4)
      .orElseBy[Rank](_.k5)

    private[model] def of(cards: List[Card]): Option[HighCard] = {
      cards.sorted match {
        case List(k1, k2, k3, k4, k5) => new HighCard(k1.rank, k2.rank, k3.rank, k4.rank, k5.rank) {}.some
        case _                        => None
      }
    }
  }

  private def combineCards(cards: List[Card]): List[List[Card]] = cards
    .groupByNel(_.rank).values.map(_.toList).toList
    .sorted(Ordering
      .by[List[Card], Int](_.size)
      .orElseBy(_.head.rank)
      .reverse
    )

  private def sameSuit(cards: Seq[Card]): Option[Suit] = {
    cards.foldRight(Set.empty[Suit])((card, set) => set + card.suit) match {
      case set if set.size == 1 => set.headOption
      case _                    => None
    }
  }

  private val possibleStraights: Set[Set[Rank]] = Set(
    Set(Ace, King, Queen, Jack, Ten),
    Set(King, Queen, Jack, Ten, Nine),
    Set(Queen, Jack, Ten, Nine, Eight),
    Set(Jack, Ten, Nine, Eight, Seven),
    Set(Ten, Nine, Eight, Seven, Six),
    Set(Nine, Eight, Seven, Six, Five),
    Set(Eight, Seven, Six, Five, Four),
    Set(Seven, Six, Five, Four, Three),
    Set(Six, Five, Four, Three, Two)
  )

  private val wheelStraight: Set[Rank] = Set(Five, Four, Three, Two, Ace)

  private def isStraight(cards: Seq[Card]): Option[Rank] = {
    cards.foldRight(SortedSet.empty[Rank])((card, set) => set + card.rank) match {
      case ranks if ranks.size == 5 && possibleStraights.contains(ranks.toSet) => ranks.maxOption
      case ranks if ranks.size == 5 && wheelStraight == ranks.unsorted         => Five.some
      case _                                                                   => None
    }
  }

}
