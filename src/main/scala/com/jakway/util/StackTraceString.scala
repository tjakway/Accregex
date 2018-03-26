package com.jakway.util

import java.io.{PrintWriter, StringWriter}

object StackTraceString {
  def stackTraceToString(t: Throwable): String = {
    //from https://stackoverflow.com/questions/1149703/how-can-i-convert-a-stack-trace-to-a-string

    val sw: StringWriter = new StringWriter()
    t.printStackTrace(new PrintWriter(sw))
    sw.toString()
  }

  def apply: Throwable => String = stackTraceToString
}
