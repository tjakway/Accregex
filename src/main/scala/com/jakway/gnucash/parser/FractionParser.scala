package com.jakway.gnucash.parser

import com.jakway.gnucash.error.ValidationError

import scala.util.{Failure, Success, Try}

object FractionParser {
  class FractionParserError(override val msg: String)
    extends ValidationError(msg)

  case object InputEmptyError
    extends ValidationError("parseFraction requires input")

  case class FormatError(override val msg: String)
    extends ValidationError(msg)

  def parseFraction(s: String): Either[ValidationError, Double] = {
    lazy val genericError = new FractionParserError(
      s"Exception thrown while" +
        s" parsing fraction")

    if(s.trim.isEmpty) {
      Left(InputEmptyError)
    } else {
      if(s.contains("/")) {
        //parse with a denominator


        val xs = s.split("/")
        if(xs.length != 2) {
          Left(FormatError(s"Expected input $s to contain at most" +
            s" 1 division sign (/)"))

        } else {
          val res = for {
            numerator <- Try(xs(0).toDouble)
            denominator <- Try(xs(1).toDouble)
            result <- Try(numerator / denominator)
          } yield {
            result
          }

          res match {
            case Success(r) => Right(r)
            case Failure(e) =>
              Left(genericError.withCause(e))
          }
        }

      } else {
        //no special processing needed
        Try(s.toDouble) match {
          case Success(d) => Right(d)
          case Failure(e) =>
            Left(genericError.withCause(e))
        }
      }
    }
  }
}
