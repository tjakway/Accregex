package com.jakway.gnucash.test

import com.jakway.gnucash.parser.ValidationError
import com.jakway.gnucash.parser.xml.{ElemReplace, NodeReplace}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node}

/**
  * transparently keep count of the number of times predicateElem returned true
  * @tparam A
  */
abstract class ElemReplaceCountPredicate[A] extends ElemReplace[A] {
  var countPredicateTrue = 0

  def predicateEC(e: Elem): Boolean

  /**
    * can't call super.predicateElem unless we're a trait...
    * @param e
    * @return
    */
  final override def predicateElem(e: Elem): Boolean = {
    if(predicateEC(e)) {
      countPredicateTrue = countPredicateTrue + 1
      true
    } else {
      false
    }
  }
}

class TestNodeReplace extends FlatSpec with Matchers {
  import com.jakway.gnucash.parser.xml.NodeTests._

  "NodeReplace" should "transform subelements" in {
    val newLabel = "book_trans"

    case class TestNodeReplaceError(override val msg: String)
      extends ValidationError(msg)

    implicit def errorType: String => ValidationError = TestNodeReplaceError.apply

    class BookReplace extends ElemReplaceCountPredicate[String] {
      override def predicateEC(e: Elem): Boolean = e.label == "book"

      override def replaceElem(e: Elem): (String, Node) = {
        (getAttribute((e, "id")).right.get, e.copy(label = newLabel))
      }
    }

    val replaceF = new BookReplace()
    val (tags, newBookTree) = replaceF.apply(BooksLiteral.books)

    replaceF.countPredicateTrue shouldEqual 2

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
