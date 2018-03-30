package com.jakway.gnucash.parser

import com.jakway.util.StackTraceString

class ValidationError(val msg: String)
  extends RuntimeException(msg) {

  val stackTrace = StackTraceString.apply(new Throwable())
}

object ValidationError {
  /**
    * TODO: alternatively, make the Throwable argument to StackTraceString.apply in the class
    * definition a parameter with default = new Throwable()
    * @param msg
    * @param cause
    * @return
    */
  def fromCause(msg: String, cause: Throwable): ValidationError = {
    return new ValidationError(msg) {
      override val stackTrace: String = StackTraceString.apply(cause)
    }
  }


  /**
    * accumulate Rights into 1 Seq unless an error occurs
    * in which case accumulate all errors
    * @param in
    * @tparam A
    * @tparam B
    * @return
    */
  def accumulateEithers[A, B](in: Seq[Either[A, Seq[B]]]):
  Either[Seq[A], Seq[B]] = {
    val empty: Either[Seq[A], Seq[B]] = Right(Seq())

    def f(accs: Either[Seq[A], Seq[B]], thisElem: Either[A, Seq[B]]):
    Either[Seq[A], Seq[B]] = (accs, thisElem) match {

      case (Left(acc), Left(q)) => Left(acc.+:(q))
      case (_, Left(q)) => Left(Seq(q))
      case (Right(acc), Right(q)) => Right(acc ++ q)
      //ignore Right's on error
      case (Left(r), _) => Left(r)
    }

    in.foldLeft(empty)(f)
  }


  def accumulateEithers[A, B](in: Seq[Either[A, B]]):
    Either[Seq[A], Seq[B]] = accumulateEithers(in.map(_.map(Seq(_))))
}
