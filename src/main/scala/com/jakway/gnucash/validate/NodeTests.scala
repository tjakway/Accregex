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

  def hasAttribute: ValidateF[(Node, String), Attribute] =
    (t: (Node, String), errorType: String => ValidationError) => {

    val (root, name) = t
    val noAttrError = Left(errorType(s"$root does not contain attribute $name"))

    def convResult(x: Option[Seq[Node]]): Either[ValidationError, Attribute] = x match {
      case None => noAttrError
      case Some(xs) if xs.isEmpty => noAttrError
      case Some(xs) => for {
        r <- onlyOne(xs)(errorType)

        //convert the possible ClassCastException into an Either
        asAttr <- Try(r.asInstanceOf[Attribute]) match {
          case Success(q) => Right(q)
          case Failure(q) => Left(errorType(getMsg((q))))
        }
      } yield {
        asAttr
      }
    }

    //find the attribute, abstracted over whether it's prefixed
    val searchRes = name.contains(namespaceSeparator) match {
      case true => {
        for {
          q <- splitXMLNameOnLastSeparator(name)(errorType)
        } yield {
          val (ns: String, attrName: String) = q
          root.attribute(ns, attrName)
        }
      }
      case false => {
        Right(root.attribute(name))
      }
    }

    searchRes.flatMap(convResult)
  }

  def findOnlyOne: ValidateF[(Node => Boolean, Node), Node] =
    (t: (Node => Boolean, Node), errorType: String => ValidationError) => {

      //not using Function.uncurried because for some reason it thinks this should take 4 parameters...

      onlyOne(XMLUtils.searchNode(t._1)(t._2))(errorType)
  }
}
