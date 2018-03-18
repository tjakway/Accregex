package com.jakway.gnucash.test

import com.jakway.util.XMLUtils
import org.scalatest._

import scala.xml.Elem

class TestXMLUtils extends FlatSpec with Matchers {

  val testElem: Elem = <testelem></testelem>

  "XMLUtils.searchNode" should "find the node when there's only 1 element" in {
    XMLUtils.searchNode(_ == testElem)(testElem) shouldEqual Seq(testElem)
  }

  //inverse of the above test
  "XMLUtils.searchNode" should "not find the node when passed !=" in {
    XMLUtils.searchNode(_ != testElem)(testElem) shouldEqual Seq()
  }
}
