package com.jakway.gnucash.validate

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

  def hasNamespace: ValidateF[(Node, String), String]
    (t: (Node, String), errorType: String => ValidationError) => {
      val (node, prefix) = t

      //we need to look up the URI of the namespace prefix from the NamespaceBinding
      //object then (if that prefix points to a valid namespace)
      //check if this node actually is in that namespace

      //look up the URI of the namespace prefix
      val nsURI = node.scope.getURI(prefix)

      //if it's null that namespace doesn't exist
      val nsExists = nsURI == null

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
    * WARNING: this method does not distinguish between attributes that don't
    * exist and attributes whose values are the empty string (it will
    * consider empty attributes to not exist)
    * @return
    */
  def getAttribute: ValidateF[(Node, String), String] =
    (t: (Node, String), errorType: String => ValidationError) => {
      val (node, attrId) = t

      val text = node \@ attrId

      if(text.trim.isEmpty) {
        Left(errorType(s"Could not find attribute $attrId in $node"))
      }
      else {
        Right(text)
      }
    }

  def findOnlyOne: ValidateF[(Node => Boolean, Node), Node] =
    (t: (Node => Boolean, Node), errorType: String => ValidationError) => {

      //not using Function.uncurried because for some reason it thinks this should take 4 parameters...

      onlyOne(XMLUtils.searchNode(t._1)(t._2))(errorType)
  }
}
