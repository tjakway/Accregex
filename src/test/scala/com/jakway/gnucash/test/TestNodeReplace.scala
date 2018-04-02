package com.jakway.gnucash.test

import com.jakway.gnucash.parser.ValidationError
import com.jakway.gnucash.parser.xml.{ElemReplace, NodeReplace}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node}

class TestNodeReplace extends FlatSpec with Matchers {
  import com.jakway.gnucash.parser.xml.NodeTests._

  "NodeReplace" should "transform subelements" in {
    val newLabel = "book_trans"

    implicit def errorType: String => ValidationError = {
      throw new RuntimeException("getAttribute shouldn't fail in TestNodeReplace")
      ???
    }

    class BookReplace extends ElemReplace[String] {
      override def predicateElem(e: Elem): Boolean = e.label == "book"

      override def replaceElem(e: Elem): (String, Node) = {
        (getAttribute((e, "id")).right.get, e.copy(label = newLabel))
      }
    }

    val (tags, newBookTree) = new BookReplace().apply(BooksLiteral.books)

    tags.length shouldEqual BooksLiteral.numBooks

    //the tags should be paired with the original nodes
    tags.foreach(_._2.label shouldEqual "book")

    tags.map(_._1).toSet shouldEqual BooksLiteral.ids

    //the root node should be unchanged
    newBookTree.label shouldEqual BooksLiteral.books.label

    //the only difference should be in the children of the root node
    newBookTree
      .asInstanceOf[Elem]
      .copy(child = BooksLiteral.books.child) shouldEqual BooksLiteral.books

    //and the children of the changed tree should have the new label
    newBookTree.child.map(_.label).toSet shouldEqual Set(newLabel)
  }
}
