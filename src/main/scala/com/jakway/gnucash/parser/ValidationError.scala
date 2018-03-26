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
}
