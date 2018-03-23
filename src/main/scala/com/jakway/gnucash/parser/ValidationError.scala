package com.jakway.gnucash.parser

class ValidationError(val msg: String)
  extends RuntimeException(msg)

object ValidationError {
}
