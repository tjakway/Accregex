package com.jakway.util

object Util {
  def anyOf[A](s: TraversableOnce[A])(f: A => Boolean): Boolean = s.foldLeft(false) {
    case (b, x) => b || f(x)
  }

  def reEither[A, B, C, D](a: Either[A, B])(fLeft: A => C)(fRight: B => D): Either[C, D] = a match {
    case Right(x) => Right(fRight(x))
    case Left(y) => Left(fLeft(y))
  }

  def either[A, B, C](a: Either[A, B])(ifLeft: A => C)(ifRight: B => C): C = a match {
    case Right(x) => ifRight(x)
    case Left(y) => ifLeft(y)
  }
}
