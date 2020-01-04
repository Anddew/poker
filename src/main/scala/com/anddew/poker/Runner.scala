package com.anddew.poker

import scala.collection.SortedMap
import scala.io.StdIn

object Runner {

  def main(args: Array[String]): Unit = {

    import Combinations.Implicits._

    val parser = if (args.contains("--omaha"))
      ??? //TODO implement omaha
    else
      new TexasHoldemParser

    val resolver = new Resolver

    LazyList
      .continually(StdIn.readLine)
      .takeWhile(_ != null)
      .foreach(submission => {
        val (board, hands) = parser.parse(submission)

        val combinations = for {
          hand <- hands
          combination = resolver.resolve(board, hand)
        } yield (hand, combination)

        combinations
          .groupBy(_._2)
          .to(SortedMap)
          .map(
            _._2
              .map(_._1)
              .map(list => list.mkString)
              .mkString("", "=", " ")
          )
          .foreach(print)
      })
  }


}
