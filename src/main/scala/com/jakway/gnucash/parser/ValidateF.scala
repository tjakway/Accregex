package com.jakway.gnucash.parser

import scala.util.{Failure, Success, Try}
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

object ValidateF {
  def getOrThrow[A](e: Either[ValidationError, A]): A = e match {
    case Right(a) => a
    case Left(e) => throw e
  }

  object MiscTests {
    /**
      * check that the passed seq is nonempty
      * @tparam A
      * @return
      */
    def notEmpty[A]: ValidateF[Seq[A], Seq[A]] =
      (s: Seq[A], errorType: String => ValidationError) => s match {
        case Seq() => Left(errorType(s"Expected $s to be nonempty"))
        case _ => Right(s)
      }


    /**
      * tests whether the trimmed string is nonempty
      * @return
      */
    def nonEmptyStr: ValidateF[String, String] =
      (s: String, errorType: String => ValidationError) => {
        if(s.trim.isEmpty) {
          Left(errorType(s"Expected $s to be nonempty"))
        } else {
          Right(s)
        }
      }

    def isNumeric: ValidateF[String, Double] =
      (str: String, errorType: String => ValidationError) => {
        Try(str.toDouble) match {
          case Success(d) => Right(d)
          case Failure(_) => Left(errorType(s"Cannot parse '$str' as a number"))
        }
    }
  }
}
