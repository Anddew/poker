package com.anddew.poker.model


sealed abstract case class Holdem private (boardHoles: Int, handHoles: Int)

object Holdem {

  final case object TexasHoldem extends Holdem(0, 0)
  final case object OmahaHoldem extends Holdem(2, 2)

}


