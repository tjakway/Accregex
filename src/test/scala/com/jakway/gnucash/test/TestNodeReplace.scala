package com.jakway.gnucash.test

import com.jakway.gnucash.parser.ValidationError
import com.jakway.gnucash.parser.xml.{ElemReplace, NodeReplace}
import com.jakway.gnucash.test.objects.BooksLiteral
import com.jakway.util.XMLUtils
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node, SpecialNode}

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

class TestNodeReplace(val regDocRoot: Node) extends FlatSpec with Matchers {
  import com.jakway.gnucash.parser.xml.NodeTests._

  case class TestNodeReplaceError(override val msg: String)
    extends ValidationError(msg)

  implicit def errorType: String => ValidationError = TestNodeReplaceError.apply

  "NodeReplace" should "transform subelements" in {
    val newLabel = "book_trans"

    class BookReplace extends ElemReplaceCountPredicate[String] {
      override def predicateEC(e: Elem): Boolean = e.label == "book"

      override def replaceElem(e: Elem): (String, Node) = {
        (getAttribute((e, "id")).right.get, e.copy(label = newLabel))
      }
    }

    val replaceF = new BookReplace()
    val (tags, newBookTree) = replaceF.doReplace(BooksLiteral.books)

    replaceF.countPredicateTrue > 0 shouldEqual true

    tags.length shouldEqual BooksLiteral.numBooks

    //the tags should be paired with the transformed nodes
    tags.foreach(_._2.label shouldEqual newLabel)

    tags.map(_._1).toSet shouldEqual BooksLiteral.ids

    //the root node should be unchanged
    newBookTree.label shouldEqual BooksLiteral.books.label

    //the only difference should be in the children of the root node
    newBookTree
      .asInstanceOf[Elem]
      .copy(child = BooksLiteral.books.child) shouldEqual BooksLiteral.books

    //and the children of the changed tree should have the new label
    XMLUtils.filterPCData(newBookTree.child)
      .map(_.label).toSet shouldEqual Set(newLabel)
  }

  /**
    * if replace is just the identity function then the tree
    * should stay the same
    */
  def testNoReplaceVisitChildren(tree: Node) = {
    abstract class IdNodeReplace extends ElemReplaceCountPredicate[String] {
      override protected def replaceElem(e: Elem): (String, Node) = (e.label, e)
    }
    class VisitOnlyChildren extends IdNodeReplace {
      override def predicateEC(e: Elem): Boolean =
      //text doesn't count as a child node
        if(e.child.filter(!_.isInstanceOf[SpecialNode]).isEmpty) {
          true
        } else {
          false
        }
      }

    val replaceF = new VisitOnlyChildren()
    val (labels, outputNodes) = replaceF.doReplace(tree)
    labels.length > 0 shouldEqual true
    outputNodes.isEmpty shouldEqual false

    tree.xml_==(outputNodes) shouldEqual true

    (labels.map(_._1), outputNodes)
  }


  it should "not change the structure of the book tree" in {
    val (labels, outputNodes) = testNoReplaceVisitChildren(BooksLiteral.books)

    labels.toSet shouldEqual Set("book")
    labels.length shouldEqual 2
  }

  it should "not change the structure of the reg doc tree" in {
    testNoReplaceVisitChildren(regDocRoot)
  }
}
