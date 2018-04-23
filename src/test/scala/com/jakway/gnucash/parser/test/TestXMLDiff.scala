package com.jakway.gnucash.parser.test

import com.jakway.gnucash.parser.xml.{FilterTransactionsDiff, HasDiffEngine, XMLEqual}
import com.jakway.util.XMLUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node


trait TestXMLDiffInstance { this: FlatSpec =>
  val node: Node
  def getEngine(originalXML: String, newXML: String): HasDiffEngine

  it should "validate a Node against itself" in {
    val res = for {
      str <- XMLUtils.nodeToString(node)
    } yield {
      getEngine(str, str).passes()
    }
    assert(res == Right(()))
  }
}

class TestFilterTransactionsDiff(val regDocXML: Node) extends FlatSpec with TestXMLDiffInstance {

  override val node: Node = regDocXML

  override def getEngine(originalXML: String, newXML: String): HasDiffEngine =
    new FilterTransactionsDiff(originalXML,
      Set(), newXML, Set(), _ => Right(null))
}

class TestXMLEqualDiff(val regDocXML: Node)
  extends FlatSpec with TestXMLDiffInstance {

  override val node: Node = regDocXML

  override def getEngine(originalXML: String, newXML: String): HasDiffEngine =
    new XMLEqual(originalXML, newXML)
}
