package com.jakway.util


/**
  * the type parameter stuff is so we can return the type of the object we're extending
  * (CRTP)
  * see https://stackoverflow.com/questions/5331722/define-method-to-return-type-of-class-extending-it
  * @tparam A
  */
trait WithCause[A <: WithCause[A]] extends RuntimeException { this: A =>
  def withCause(t: Throwable): A = {
    //use addSuppressed instead of initCause
    // -thread safe
    // -can be called more than once
    addSuppressed(t)
    this
  }

  val stackTrace = StackTraceString.apply(new Throwable())
}
