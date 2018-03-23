package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.{Parser, ValidationError}

import scala.xml.XML

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  val parser = new Parser

  /**
    * for errors before tests are run
    * @param msg
    */
  case class TestParserLoadError(msg: String) extends ValidationError

  val regDocRoot = XML.load(getClass.getResource(regDocResource))


  val book = parser.findBookNode(regDocRoot)(TestParserLoadError).right.get

  "The Parser" should "load the number of accounts" in {
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }
}
