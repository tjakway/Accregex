package com.jakway.gnucash.test

import org.scalatest.{FlatSpec, Matchers}

import scala.xml.{Elem, Node}

class TestXMLAPI(val root: Node) extends FlatSpec with Matchers {

  "Scala's XML API" should "respect left fold identity over children" in {
    val zero = Elem(null, root.label,
      root.attributes, root.scope, true)
    val foldRes = root.foldLeft(zero){
      //don't copy the root node
      /*case (n, thisN) if thisN == root => {
        println("hit root node")
        n
      }*/
      case (n, thisN) => {
        n.copy(child = n.child.+:(thisN))
      }
    }

    def id[A]: A => A = x => x
    val foldSeq = root.foldLeft(Seq(): Seq[Node]) {
      (acc, thisN) => acc.+:(thisN)
    }

    val mapSeq = root.map(id)

    foldSeq shouldEqual mapSeq

    foldRes.asInstanceOf[Node] shouldEqual root
  }
}
