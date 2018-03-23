package com.jakway.gnucash.parser

class ValidationError(val msg: String)
  extends RuntimeException(msg)

object ValidationError {
  def getOrThrow[A](e: Either[ValidationError, A]): A = e match {
    case Right(a) => a
    case Left(e) => throw e
  }
}
