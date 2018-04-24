package com.jakway.util.error

import com.jakway.util.StackTraceString

trait WithCause[+A] extends RuntimeException { this: A =>
  def withCause(t: Throwable): A = {
    //use addSuppressed instead of initCause
    // -thread safe
    // -can be called more than once
    addSuppressed(t)
    this
  }

  val stackTrace = StackTraceString.apply(new Throwable())
}
