package com.anddew.poker

// TODO replace with Order
package object model {

  implicit val rankOrdering: Ordering[Rank] = Ordering.by(_.priority)

  implicit val cardOrdering: Ordering[Card] = Ordering.by(_.rank)

  implicit def listOrdering[A](implicit ord: Ordering[A]): Ordering[List[A]] =
    (left: List[A], right: List[A]) => {
      if (left.isEmpty) 0 else {
        val result = Ordering[A].compare(left.head, right.head)
        if (result != 0) result else Ordering[List[A]].compare(left.tail, right.tail)
      }
    }

  // TODO upgrade to match cases to check each Combination subtype
  implicit val combinationOrdering: Ordering[Combination] = Ordering.by[Combination, Int](_.weight)
  implicit val handCombinationOrdering: Ordering[HandCombination] = Ordering.by[HandCombination, Combination](_.combination)

}
