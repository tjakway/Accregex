package com.jakway.gnucash.parser

import com.jakway.util.XMLUtils

import scala.util.{Failure, Success, Try}
import scala.xml.{Attribute, Node, NodeSeq}

/**
  * the downside to using SAM here is that to call other ValidationF's from a ValidationF we need to
  * either re-declare errorType as an implicit or pass it explicitly
  */
object NodeTests {
  import ValidationError._

  val namespaceSeparator = ":"

  def onlyOne[A]: ValidateF[Seq[A], A] =
    (s: Seq[A], errorType: String => ValidationError) => {

    val msg = "expected only 1 item in seq but got: "
    if (s.length == 1) {
      Right(s.head)
    } else if (s.isEmpty) {
      Left(errorType(msg + "empty seq"))
    }
    else {
      Left(errorType(msg + s.toString))
    }
  }

  /**
    * XMLUtils.searchNode with ValidateF
    * returns Left if no elems are found
    * @return
    */
  def getElems: ValidateF[(Node, String), Seq[Node]] =
    (t: (Node, String), errorType: String => ValidationError) => {

      val (node, name) = t

      XMLUtils.searchNode(_.label == name)(node) match {
        //empty Seq is an error
        case Seq() => Left(errorType(s"did not find any nodes named $name"))
        case x => Right(x)
      }
    }

  /**
    * getElems + onlyOne
    * @return
    */
  def getElem: ValidateF[(Node, String), Node] =
    (t: (Node, String), errorType: String => ValidationError) =>
      getElems(t)(errorType).flatMap(onlyOne(_)(errorType))

  /**
    * @return the (trimmed) text of the Node or Left if there isn't any
    */
  def getNodeText: ValidateF[Node, String] =
    (n: Node, errorType: String => ValidationError) => {
      val text = n.text.trim
      if(text.isEmpty) {
        Left(errorType(s"$n has no text"))
      } else {
        Right(text)
      }
  }

  def hasSubNodes: ValidateF[(Node, String), NodeSeq] =
    //scala lacks automatic tuple unboxing so we have to do it manually
    //it also doesn't have variadic type parameters which would obviate the
    //need for this
    (t: (Node, String), errorType: String => ValidationError) => {

    val (root: Node, name: String) = t

    val sub = (root \ name)
    sub.isEmpty match {
      case true => Left(errorType(s"could not find subnode of $root named $name"))
      case false => Right(sub)
    }
  }

  def hasSubNode: ValidateF[(Node, String), Node] =
    (t: (Node, String), errorType: String => ValidationError) => {

    for {
      h <- hasSubNodes(t)(errorType)
      r <- onlyOne(h)(errorType)
    } yield {
      r
    }
  }

  private def splitXMLNameOnLastSeparator: ValidateF[String, (String, String)] =
    (s: String, errorType: String => ValidationError) => {

    val splitIndex = s.lastIndexOf(namespaceSeparator)
    val prefix = s.substring(0, splitIndex)
    if(splitIndex >= (s.length - 1)) {
      Left(errorType("You cannot search for an attribute with a namespace and no name"))
    } else {
      val suffix = s.substring(splitIndex + 1, s.length - 1)
      Right((prefix, suffix))
    }
  }

  /**
    * @return the URL of that namespace if this node is within it
    */
  def hasNamespace: ValidateF[(Node, String), String] =
    (t: (Node, String), errorType: String => ValidationError) => {
      val (node, prefix) = t

      //we need to look up the URI of the namespace prefix from the NamespaceBinding
      //object then (if that prefix points to a valid namespace)
      //check if this node actually is in that namespace

      //look up the URI of the namespace prefix
      val nsURI = node.scope.getURI(prefix)

      //if it's null that namespace doesn't exist
      val nsExists = nsURI != null

      if(nsExists) {
        //check if this node has that namespace
        if(node.namespace == nsURI) {
          Right(nsURI)
        } else {
          Left(errorType(s"node.namespace was ${node.namespace} but expected $nsURI"))
        }
      } else {
        Left(errorType(s"No namespace with prefix $prefix exists"))
      }
  }

  /**
    * @return
    */
  def getAttribute: ValidateF[(Node, String), String] =
    (t: (Node, String), errorType: String => ValidationError) => {
      val (node, attrId) = t

      //the keys in asAttrMap are correctly prefixed with namespaces
      //like you would expect
      //but \@ will NOT handle prefixed attributes!

      node.attributes.asAttrMap.get(attrId) match {
        case None => Left(errorType(s"Could not find attribute $attrId in $node"))
        case Some(text) => Right(text)
      }
    }

  /**
    * like getAttribute but return the Node instead of the text of the attribute
    * @return
    */
  def withAttribute: ValidateF[(Node, String), Node] =
    (t: (Node, String), errorType: String => ValidationError) => {
      for {
        _ <- getAttribute(t)(errorType)
      } yield (t._1)
    }

  /**
    * @param t: (Node, attribute name, expected attribute text)
    * @return the original node on success
    */
  def expectAttribute: ValidateF[(Node, String, String), Node] =
    (t: (Node, String, String), errorType: String => ValidationError) => {
      val (n, attr, expectedAttrValue) = t

        getAttribute((n, attr))(errorType).flatMap(attrValue => {
          if(attrValue == expectedAttrValue) {
            Right(n)
          } else {
            Left(errorType("expected attribute $attr of $n to have value $expectedAttrValue but got $attrValue"))
          }
        })
      }

  def findOnlyOne: ValidateF[(Node => Boolean, Node), Node] =
    (t: (Node => Boolean, Node), errorType: String => ValidationError) => {

      //not using Function.uncurried because for some reason it thinks this should take 4 parameters...

      onlyOne(XMLUtils.searchNode(t._1)(t._2))(errorType)
  }
}
