package com.anddew.poker

import cats.Order
import com.anddew.poker.model.Combination._


package object model {

  // TODO move to companions
  implicit val rankOrdering: Ordering[Rank] = Ordering.by[Rank, Int](_.weight).reverse
  implicit val rankOrder: Order[Rank] = Order.fromOrdering

  implicit val cardOrdering: Ordering[Card] = Ordering.by[Card, Rank](_.rank)
  implicit val cardOrder: Order[Card] = Order.fromOrdering

  // TODO exists generic listorder in Order?
  implicit def listOrdering[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    (left: List[A], right: List[A]) => {
      if (left.isEmpty) 0 else {
        val result = Ordering[A].compare(left.head, right.head)
        if (result != 0) result else Ordering[List[A]].compare(left.tail, right.tail)
      }
    }

  implicit val handCombinationOrdering: Ordering[HandCombination] = Ordering.by[HandCombination, Combination](_.combination)

}
