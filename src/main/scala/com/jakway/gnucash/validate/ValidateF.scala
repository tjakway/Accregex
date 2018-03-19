package com.jakway.gnucash.validate

import scala.xml.Node

//package private validation function class
//see https://stackoverflow.com/questions/49353695/type-synonyms-for-implicits
private[validate] abstract class ValidateF {
  // single abstract method without implicits
  def apply_impl(
                  f: Node => Boolean,
                  n: Node,
                  errorType: String => ValidationError
                ): Either[ValidationError, Node]

  // actual `apply` with implicits
  def apply
  (f: Node => Boolean)
  (n: Node)
  (implicit errorType: String => ValidationError)
  : Either[ValidationError, Node] = {
    apply_impl(f, n, errorType)
  }
}
