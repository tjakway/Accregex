package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.{Parser, ValidationError}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Node, XML}

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  val parser = new Parser

  /**
    * for errors before tests are run
    * @param msg
    */
  case class TestParserLoadError(override val msg: String) extends ValidationError(msg)

  val regDocRoot = XML.load(getClass.getResource(regDocResource))


  val book =
    parser.findBookNode(regDocRoot)(TestParserLoadError.apply _).right.get


  "The Parser" should "load the number of accounts" in {
    //TODO: parameterize expected value
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }
}

class TestParserRegDocXML extends TestParser("/reg_doc_example.gnucash")
