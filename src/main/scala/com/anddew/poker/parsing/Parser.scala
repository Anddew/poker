package com.anddew.poker.parsing

import cats.data.EitherNel
import com.anddew.poker.error.ParsingError


trait Parser[A, B] {

  def parse(input: A): EitherNel[ParsingError, B]

}

object Parser {

  def parse[A, B](input: A)(implicit parser: Parser[A, B]): EitherNel[ParsingError, B] = parser.parse(input)

}