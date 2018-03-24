package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser._
import com.jakway.util.XMLUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Node, XML}

class TestParser(val regDocResource: String) extends FlatSpec with Matchers {
  import NodeTests._
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

  "XMLUtils.searchNode" should "find the 3 count-data nodes" in {
    val foundNodes = XMLUtils.searchNode(_.label == "count-data")(book)
    foundNodes.length shouldEqual 3
    foundNodes.map(_.text.toInt).sorted shouldEqual Seq(1, 2, 65)
  }

  "The Parser" should "extract the book node" in {
    book.label shouldEqual "book"
    hasNamespace((book, "gnc")).isRight shouldEqual true
    expectAttribute((book, "version", "2.0.0")).isRight shouldEqual true
  }

  "The Parser" should "load the number of accounts" in {
    //TODO: parameterize expected value
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }

  "The Parser" should "load the number of transactions" in {
    //TODO: parameterize expected value
    parser.extractNumTransactions(book) shouldEqual Right(2)
  }

  "NodeTests" should "detect namespaces" in {
    val nsUrl = "http://example.com/namespace"
    val e: Node = {
      val testNode: Node = <rootnode xmlns:foo={nsUrl}>
          <foo:bar></foo:bar>
        </rootnode>

      ValidateF.getOrThrow(NodeTests.getElem((testNode, "bar")))
    }

    hasNamespace((e, "foo"))(x => new TestParserError("wrong namespace: " + x)) shouldEqual
      Right(nsUrl)
  }

  "The Parser" should "parse the root account node" in {
    val accs = parser.parseAccountNodes(regDocRoot)

    //the root account in reg_doc_example.gnucash
    val rootAccount = UnlinkedAccount("2.0.0",
                        "f52c28f32edd309e768494995470343b",
                        "Root Account",
                        "ROOT",
                        None, None)

    accs.right.get.filter(_.parentId.isEmpty) shouldEqual Seq(rootAccount)
  }
}

class TestParserRegDocXML extends TestParser("/reg_doc_example.gnucash")
