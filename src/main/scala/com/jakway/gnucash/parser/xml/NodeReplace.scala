package com.jakway.gnucash.parser.xml

import scala.xml.{Elem, Node}

abstract class NodeReplace[A] {
  def predicate(n: Node): Boolean

  def replace(n: Node): (A, Node)

  def apply(tree: Node): (Seq[(A, Node)], Node) =
    NodeReplace.tagAndReplaceNodes(predicate)(replace)(tree)
}

/**
  * ignores non-Elem Nodes
  * @tparam A
  */
abstract class ElemReplace[A] extends NodeReplace[A] {
  final override def predicate(n: Node): Boolean = n match {
    case e: Elem => predicateElem(e)
    case _ => false
  }

  final override def replace(n: Node): (A, Node) = n match {
    case e: Elem => replaceElem(e)
    case _ => throw new RuntimeException("ElemReplace.replace " +
      "should never be called on a non-Elem")
  }

  def predicateElem(e: Elem): Boolean
  def replaceElem(e: Elem): (A, Node)
}

/**
  * decouple logging from replace logic
  * @param log
  * @tparam A
  */
abstract class LoggingElemReplace[A](val log: Elem => A)
  extends ElemReplace[A] {

  def replaceE(e: Elem): Node

  /**
    * will only be called when the predicate returns true
    * @param e
    * @return
    */
  final override def replaceElem(e: Elem): (A, Node) = {
    (log(e), replaceE(e))
  }
}

object NodeReplace {
  def tagAndReplaceNodes[A](predicate: Node => Boolean)
                  (replace: Node => (A, Node))
                  (tree: Node): (Seq[(A, Node)], Node) = {


    var tags: Seq[(A, Node)] = Seq()

    //note: the predicate may get called >1 per node but
    //replace will only be called once
    def applyTrans(n: Node): Node =
      if(predicate(n)) {
        val r = replace(n)
        tags = tags.+:(r)
        r._2
      } else {
        n
      }

    def rec(n: Node): Node = n match {
      case e: Elem if(predicate(e)) => applyTrans(e)
      case e: Elem if(!predicate(e)) => {
        e.copy(child = e.child.map(rec))
      }
      case _ => n
    }

    val res = rec(tree)

    (tags, res)
  }
}
