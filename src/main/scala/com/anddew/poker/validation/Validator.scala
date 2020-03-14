package com.anddew.poker.validation

import cats.data.ValidatedNel
import com.anddew.poker.error.ValidationError


trait Validator[A] {

  def validate(input: A): ValidatedNel[ValidationError, A]

}

object Validator {

  def validate[A](input: A)(implicit validator: Validator[A]): ValidatedNel[ValidationError, A] = validator.validate(input)

}
