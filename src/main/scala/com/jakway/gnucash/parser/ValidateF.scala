package com.jakway.gnucash.parser

import scala.xml.Node

//package private validation function class
//see https://stackoverflow.com/questions/49353695/type-synonyms-for-implicits
private[parser] abstract class ValidateF[I,O] {
  // single abstract method without implicits
  def validate( i: I,
                  errorType: String => ValidationError
                ): Either[ValidationError, O]

  // actual `apply` with implicits
  def apply
  (i: I)
  (implicit errorType: String => ValidationError)
  : Either[ValidationError, O] = {
    validate(i, errorType)
  }
}
