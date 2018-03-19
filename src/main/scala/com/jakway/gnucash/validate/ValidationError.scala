package com.jakway.gnucash.validate

trait ValidationError

object ValidationError {
  def getMsg(t: Throwable): String = {
    //TODO: get stack trace & msg
    ???
  }
}