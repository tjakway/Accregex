package com.jakway.gnucash.parser

import com.jakway.util.StackTraceString

class ValidationError(val msg: String)
  extends RuntimeException(msg) {

  val stackTrace = StackTraceString.apply(new Throwable())
}

class MultiValidationError(val errors: Seq[ValidationError])
  extends ValidationError(s"Errors: $errors")

object MultiValidationError {
  def wrap[B](in: Either[Seq[ValidationError], Seq[B]]):
    Either[ValidationError, Seq[B]] = in match {
    case Left(xs) => Left(new MultiValidationError(xs))
    case Right(xs) => Right(xs)
  }
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


  def accumulateEithersSimpleSeq[A, B](in: Seq[Either[A, B]]):
    Either[Seq[A], Seq[B]] = accumulateEithers(in.map(_.map(Seq(_))))

  def accumulateEithersNestedSeq[A, B](in: Seq[Either[Seq[A], Seq[B]]]):
    Either[Seq[A], Seq[B]] = {
    val empty: Seq[Either[A, Seq[B]]] = Seq()

    //flatten each Left[Seq[A]] into a Seq[Left[A]]
    val flattenedLefts: Seq[Either[A, Seq[B]]] = in.foldLeft(empty) {
      case (acc, Right(xs)) => acc.+:(Right(xs))
      case (acc, Left(xs)) => acc ++ xs.map(Left(_))
    }

    accumulateEithers(flattenedLefts)
  }

  def accumulateAndWrap[B](in: Seq[Either[ValidationError, B]]):
    Either[ValidationError, Seq[B]] = {
    accumulateEithersSimpleSeq(in) match {
      case Left(xs) => Left(new MultiValidationError(xs))
      case Right(xs) => Right(xs)
    }
  }
}
