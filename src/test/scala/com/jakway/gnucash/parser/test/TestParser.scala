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


  object RegDocNodes {
    object OpeningBalancesAccount {
      lazy val node = {
        val n = <gnc-v2
        xmlns:gnc="http://www.gnucash.org/XML/gnc"
        xmlns:act="http://www.gnucash.org/XML/act"
        xmlns:book="http://www.gnucash.org/XML/book"
        xmlns:cd="http://www.gnucash.org/XML/cd"
        xmlns:cmdty="http://www.gnucash.org/XML/cmdty"
        xmlns:price="http://www.gnucash.org/XML/price"
        xmlns:slot="http://www.gnucash.org/XML/slot"
        xmlns:split="http://www.gnucash.org/XML/split"
        xmlns:sx="http://www.gnucash.org/XML/sx"
        xmlns:trn="http://www.gnucash.org/XML/trn"
        xmlns:ts="http://www.gnucash.org/XML/ts"
        xmlns:fs="http://www.gnucash.org/XML/fs"
        xmlns:bgt="http://www.gnucash.org/XML/bgt"
        xmlns:recurrence="http://www.gnucash.org/XML/recurrence"
        xmlns:lot="http://www.gnucash.org/XML/lot"
        xmlns:job="http://www.gnucash.org/XML/job"
        xmlns:invoice="http://www.gnucash.org/XML/invoice"
        xmlns:addr="http://www.gnucash.org/XML/addr"
        xmlns:cust="http://www.gnucash.org/XML/cust"
        xmlns:billterm="http://www.gnucash.org/XML/billterm"
        xmlns:bt-days="http://www.gnucash.org/XML/bt-days"
        xmlns:bt-prox="http://www.gnucash.org/XML/bt-prox"
        xmlns:taxtable="http://www.gnucash.org/XML/taxtable"
        xmlns:tte="http://www.gnucash.org/XML/tte"
        xmlns:order="http://www.gnucash.org/XML/order"
        xmlns:employee="http://www.gnucash.org/XML/employee"
        xmlns:entry="http://www.gnucash.org/XML/entry"
        xmlns:owner="http://www.gnucash.org/XML/owner"
        xmlns:vendor="http://www.gnucash.org/XML/vendor">
          <gnc:account version="2.0.0">
            <act:name>Opening Balances</act:name>
            <act:id type="guid">99fa355648ceb345777b6c968f46f6aa</act:id>
            <act:type>EQUITY</act:type>
            <act:commodity>
              <cmdty:space>ISO4217</cmdty:space>
              <cmdty:id>USD</cmdty:id>
            </act:commodity>
            <act:commodity-scu>100</act:commodity-scu>
            <act:description>Opening Balances</act:description>
            <act:parent type="guid">3bceb6ba629ba0e51430abcea01bc95f</act:parent>
          </gnc:account>
        </gnc-v2>

        onlyOne(n.child.filter(q => q.label == "account" && hasNamespace((q, "gnc")).isRight ))
          .toOption.get
      }

      val expected = UnlinkedAccount("2.0.0",
        "99fa355648ceb345777b6c968f46f6aa",
        "Opening Balances",
        "EQUITY",
        Some("Opening Balances"),
        Some("3bceb6ba629ba0e51430abcea01bc95f"))
    }
  }

  val book =
    parser.findBookNode(regDocRoot)(TestParserLoadError.apply _).right.get

  "XMLUtils.searchNode" should "find the 3 count-data nodes" in {
    val foundNodes = XMLUtils.searchNode(_.label == "count-data")(book)
    foundNodes.length shouldEqual 3
    foundNodes.map(_.text.toInt).sorted shouldEqual Seq(1, 2, 65)
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

    accs.map(_.filter(_.parentId.isEmpty)) shouldEqual Right(Seq(rootAccount))
  }


  it should "extract the book node" in {
    book.label shouldEqual "book"
    hasNamespace((book, "gnc")).isRight shouldEqual true
    expectAttribute((book, "version", "2.0.0")).isRight shouldEqual true
  }

  it should "load the number of accounts" in {
    //TODO: parameterize expected value
    parser.extractNumAccounts(book) shouldEqual Right(65)
  }

  it should "load the number of transactions" in {
    //TODO: parameterize expected value
    parser.extractNumTransactions(book) shouldEqual Right(2)
  }

  it should "parse the opening balance node" in {
    import RegDocNodes._

    val n = parser.parseAccountNode(OpeningBalancesAccount.node)
    n shouldEqual Right(OpeningBalancesAccount.expected)
  }

  it should "link accounts properly" in {
    val accs = parser
      .parseAccountNodes(regDocRoot)
      .flatMap(Parser.linkAccounts)
      .right.get

    val liabilities = accs.filter(_.name == "Liabilities").head
    val accountsPayable = accs.filter(_.name == "Accounts Payable").head

    val root = accs.filter(_.name == "Root Account").head

    root.parent shouldEqual None

    liabilities.parent shouldEqual Some(root)
    accountsPayable.parent shouldEqual Some(liabilities)


  }
}

class TestParserRegDocXML extends TestParser("/reg_doc_example.gnucash")
