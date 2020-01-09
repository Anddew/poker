package com.anddew.poker

import com.anddew.poker.model.{HandCombination, Holdem, OmahaHoldem, TexasHoldem}

import scala.collection.SortedMap
import scala.io.StdIn

object Runner {

  val OMAHA_HOLDEM_OPTION = "--omaha"
  val EQUAL_SIGN = "="
  val WHITESPACE = " "

  def main(args: Array[String]): Unit = {

    implicit val holdem: Holdem = if (args.contains(OMAHA_HOLDEM_OPTION)) OmahaHoldem else TexasHoldem

    val parser = new HoldemParser
    val resolver = new Resolver

    LazyList
      .continually(StdIn.readLine)
      .takeWhile(_ != null)
      .map(submission => {
        val (board, hands) = parser.parse(submission)

        val combinations = for {
          hand <- hands
          combination = resolver.resolve(board, hand)
        } yield HandCombination(hand, combination)

        combinations
          .groupMap(_.combination)(_.hand)
          .view
          .mapValues(
            handsGroup => handsGroup
              .map(_.toString)
              .sorted
              .mkString(EQUAL_SIGN)
          )
          .to(SortedMap)
          .values
          .mkString(WHITESPACE)
      })
      .foreach(println)
  }

}
