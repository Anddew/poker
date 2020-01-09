package com.anddew.poker.model

sealed abstract class Holdem(val boardHoles: Int, val handHoles: Int)

case object TexasHoldem extends Holdem(0, 0)
case object OmahaHoldem extends Holdem(2, 2)
