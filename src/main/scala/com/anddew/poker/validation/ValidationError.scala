package com.anddew.poker.validation

sealed abstract class ValidationError(message: String) extends Throwable(message) {
  override def toString: String = message
}

object ValidationError {
  case class IllegalLength(input: String) extends ValidationError(s"Illegal length: <$input>")
}
