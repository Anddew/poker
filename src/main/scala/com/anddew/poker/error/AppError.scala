package com.anddew.poker.error


sealed abstract class AppError(message: String) extends Throwable(message) {
  override def toString: String = message
}


sealed abstract class ParsingError(message: String) extends AppError(message)

object ParsingError {
  case object EmptyInput extends ParsingError("Input is empty.")
  case class InvalidInput(message: String) extends ParsingError(s"Invalid input: $message")
  case class IllegalSymbol(message: String) extends ParsingError(s"Illegal symbol: $message")
}


sealed abstract class ValidationError(message: String) extends AppError(message)

object ValidationError {
  case class IllegalLength(input: String) extends ValidationError(s"Illegal length: $input")
}
