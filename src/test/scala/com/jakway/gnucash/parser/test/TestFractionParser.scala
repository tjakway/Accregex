package com.jakway.gnucash.parser.test

import org.scalatest.{FlatSpec, Matchers}

class TestFractionParser extends FlatSpec with Matchers {
  import com.jakway.gnucash.parser.FractionParser._

  "The Fraction Parser" should "parse a basic fraction" in {
    parseFraction("100/100") shouldEqual Right(1: Double)
  }

  /**
    * this is mandated by IEEE 754
    * see e.g. https://stackoverflow.com/questions/12954193/why-does-division-by-zero-with-floating-point-or-double-precision-numbers-not
    */
  it should "return infinity when dividing by zero" in {
    parseFraction("100/0") shouldEqual Right(Double.PositiveInfinity)
  }


  it should "handle negatives in the numerator" in {
    parseFraction("-100000/100") shouldEqual Right((-1 * (100000 / 100)): Double)
  }

  it should "handle numerator=0" in {
    parseFraction("0/100000") shouldEqual Right(0: Double)
  }
}
