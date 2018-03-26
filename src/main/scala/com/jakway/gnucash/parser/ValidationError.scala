package com.jakway.gnucash.parser

import com.jakway.util.StackTraceString

class ValidationError(val msg: String)
  extends RuntimeException(msg) {

  val stackTrace = StackTraceString.apply(new Throwable())
}

object ValidationError {
}
