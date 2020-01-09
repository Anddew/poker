package com.anddew.poker

package object model {

  implicit def rankOrdering: Ordering[Rank] = Ordering.by(_.priority)

  implicit def cardOrdering: Ordering[Card] = Ordering.by(_.rank)

  implicit def listOrdering[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    (left: List[A], right: List[A]) => {
      if (left.isEmpty) 0 else {
        val result = Ordering[A].compare(left.head, right.head)
        if (result != 0) result else Ordering[List[A]].compare(left.tail, right.tail)
      }
    }

  implicit def combinationOrdering: Ordering[Combination] = Ordering
    .by[Combination, Int](_.weight)
    .orElseBy(_.kickers)

}
