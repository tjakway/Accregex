package com.jakway.gnucash.test

import com.jakway.util.XMLUtils
import org.scalatest._

import scala.xml.{Elem, Node}

class TestXMLUtils extends FlatSpec with Matchers {

  val testElem: Elem = <testelem></testelem>

  //verbatim from https://github.com/scala/scala-xml/wiki/Getting-started
  val books = <books>
      <book id="b1615">Don Quixote</book>
      <book id="b1867">War and Peace</book>
    </books>

  "XMLUtils.searchNode" should "find the node when there's only 1 element" in {
    XMLUtils.searchNode(_ == testElem)(testElem) shouldEqual Seq(testElem)
  }

  //inverse of the above test
  it should "not find the node when passed !=" in {
    XMLUtils.searchNode(_ != testElem)(testElem) shouldEqual Seq()
  }

  //if querying for subelements in terms of the root element we should return
  //the root element
  //i.e. "contains a book" is a property of the node ABOVE book, not the book
  //node itself (unless that book node contains another book node)
  it should "return the root element when querying for subelements" in {
    def hasBook(n: Node) = (n \ "book").isEmpty

    XMLUtils.searchNode(hasBook)(books)
  }

  it should "allow separation by attribute" in {
    //annoyingly Node.attribute returns an Option[Seq[Node]] instead of just a Seq[Node
    def isDonQuixote(n: Node) = !(n.attribute("id")
      .getOrElse(Seq())
      .filter(_.text == "b1615")
      .isEmpty)

    //TODO: no idea why Don Quixote is at index 1...
    XMLUtils.searchNode(isDonQuixote)(books) shouldEqual Seq(books.child(1))
  }
}
