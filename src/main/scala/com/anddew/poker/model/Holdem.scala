package com.anddew.poker.model


sealed trait Holdem {
  val boardSize: Int
  val boardHoles: Int
  val handSize: Int
  val handHoles: Int
}

object Holdem {

  final case object TexasHoldem extends Holdem {
    val boardSize: Int = 5
    val boardHoles: Int = 0
    val handSize: Int = 2
    val handHoles: Int = 0
  }

  final case object OmahaHoldem extends Holdem {
    val boardSize: Int = 5
    val boardHoles: Int = 2
    val handSize: Int = 4
    val handHoles: Int = 2
  }

}


