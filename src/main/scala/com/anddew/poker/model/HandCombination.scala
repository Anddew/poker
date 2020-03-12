package com.anddew.poker.model

import cats.Order


final case class HandCombination(hand: Hand, combination: Combination)

object HandCombination {

  implicit val handCombinationOrdering: Ordering[HandCombination] = Ordering.by[HandCombination, Combination](_.combination)
  implicit val handCombinationOrder: Order[HandCombination] = Order.fromOrdering

}
