package com.jakway.gnucash.parser.xml

import scala.xml.Node

object NodeReplace {
  def tagAndReplaceNodes[A](predicate: Node => Boolean)
                  (replace: Node => (A, Node))
                  (tree: Node): (Seq[A], Node) = {


    ???
    //tree.foldLeft()
  }
}
