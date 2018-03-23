package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.{NodeTests, Parser, ValidateF, ValidationError}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Node, XML}

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  val parser = new Parser

  /**
    * for errors before tests are run
    * @param msg
    */
  class TestParserError(override val msg: String) extends ValidationError(msg)
  case class TestParserLoadError(override val msg: String) extends TestParserError(msg)

  implicit def errorType: String => ValidationError = new TestParserError(_)

  val regDocRoot = XML.load(getClass.getResource(regDocResource))


  val book =
    parser.findBookNode(regDocRoot)(TestParserLoadError.apply _).right.get


  "The Parser" should "load the number of accounts" in {
    //TODO: parameterize expected value
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }

  "NodeTests" should "detect namespaces" in {
    val nsUrl = "http://example.com/namespace"
    val e: Node = {
      val testNode: Node = <rootnode xmlns:foo={nsUrl}>
          <foo:bar></foo:bar>
        </rootnode>

      ValidateF.getOrThrow(NodeTests.getElem((testNode, "bar")))
    }

    NodeTests.hasNamespace((e, "foo"))(x => new TestParserError("wrong namespace: " + x)) shouldEqual
      Right(nsUrl)
  }
}

class TestParserRegDocXML extends TestParser("/reg_doc_example.gnucash")
