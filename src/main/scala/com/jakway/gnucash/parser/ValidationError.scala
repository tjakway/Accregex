package com.jakway.gnucash.parser

trait ValidationError

object ValidationError {
  def getMsg(t: Throwable): String = {
    //TODO: get stack trace & msg
    ???
  }
}