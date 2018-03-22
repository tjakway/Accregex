package com.jakway.gnucash.test

import com.jakway.gnucash.parser.{ValidationError, Parser}
import com.jakway.util.XMLUtils
import org.scalatest._

import scala.xml.XML

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  import Parser._

  /**
    * for errors before tests are run
    * @param msg
    */
  case class TestParserLoadError(msg: String) extends ValidationError

  val regDocRoot = XML.load(getClass.getResource(regDocResource))


  val book = findBookNode(regDocRoot)(TestParserLoadError).right.get

  "The Parser" should "load the number of accounts" in {
    extractNumAccounts()
  }
}
