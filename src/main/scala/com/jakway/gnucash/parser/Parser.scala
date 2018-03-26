package com.jakway.gnucash.parser

import com.jakway.util.XMLUtils

import scala.util.Try
import scala.xml.Node

case class UnlinkedAccount(version: String,
                   id: String,
                   name: String,
                   accType: String,
                   description: Option[String],
                   parentId: Option[String]) {
  def link(parent: Option[LinkedAccount]) = {
    LinkedAccount(version, id, name, accType, description, parent)
  }
}

case class LinkedAccount(version: String,
                   id: String,
                   name: String,
                   accType: String,
                   description: Option[String],
                   parent: Option[LinkedAccount])

case class Transaction(version: String, id: String)

class Parser {
  import NodeTests._

  private[parser] def findBookNode: ValidateF[Node, Node] =
    (root, errorType: String => ValidationError) => {
      def isBookNode(n: Node): Boolean = {
        n.label == "book" &&
          getAttribute((n, "version"))(errorType).isRight &&
          hasNamespace((n, "gnc"))(errorType).isRight
      }

      findOnlyOne((isBookNode _, root))(errorType)
    }

  private def isCountDataNode(n: Node): Boolean = {
    //check if it's a number
    Try(n.text.toInt).isSuccess &&
      //and has the expected attribute and namespace
      getAttribute((n, "cd:type"))(new ValidationError(_)).isRight &&
      hasNamespace((n, "gnc"))(new ValidationError(_)).isRight
  }


  /**
    * @param operand what we're counting (accounts or transactions)
    * @return
    */
  private def extractNumNode(operand: String): ValidateF[Node, Int] =
    (book: Node, e: String => ValidationError) => {
    implicit def errorType: String => ValidationError = e

    //find the right node and extract the text
    val foundNodes: Either[ValidationError, String] =
      getElems((book, "count-data"))
        .map { elems =>
          elems
            .filter(q => hasNamespace.apply((q, "gnc")).isRight &&
              expectAttribute.apply((q, "cd:type", operand)).isRight)
        }
      .flatMap(onlyOne.apply)
      .flatMap(getNodeText.apply)

      /*onlyOne(XMLUtils.searchNode(isCountDataNode)(book)(errorType)
      .filter(n => expectAttribute((n, "cd:type", operand))(errorType).isRight))
      .flatMap(getNodeText.apply _)*/

    //convert the text to an integer and return it
    foundNodes match {
      case Right(node) => Try(node.toInt).toEither match {
        case Left(t) => Left(errorType(s"Could not convert text of node $book '$node' to an integer"))
        case Right(q) => Right(q)
      }

      case Left(q) => Left(q)
    }
  }

  def extractNumAccounts: Node => Either[ValidationError, Int] = {
    case class ExtractNumAccountsError(override val msg: String) extends ValidationError(msg)
    extractNumNode("account")(_)(ExtractNumAccountsError.apply _)
  }


  def extractNumTransactions: Node => Either[ValidationError, Int] = {
    case class ExtractNumTransactionsError(override val msg: String) extends ValidationError(msg)
    extractNumNode("transaction")(_)(ExtractNumTransactionsError.apply _)
  }

  def parseAccountNode(n: Node): Either[ValidationError, UnlinkedAccount] = {
    case class ParseAccountNodeError(override val msg: String) extends ValidationError(msg)
    implicit def errorType: String => ValidationError = ParseAccountNodeError.apply

    for {
      _ <- hasNamespace((n, "gnc"))
      version <- getAttribute((n, "version"))

      accountNameNode <- getElem((n, "name"))
      accountName <- getNodeText(accountNameNode)

      //id node of type guid
      idNode <- queryElem((n, { x =>
        x.label == "id" &&
        expectAttribute((x, "type", "guid")).isRight &&
        hasNamespace((x, "act")).isRight
      }))


      id <- getNodeText(idNode)

      accountType <- getElem((n, "type")).flatMap(getNodeText.apply)

    } yield {
      //retrieve the description if there is one
      val description = getElem((n, "description")).flatMap(getNodeText.apply).toOption

      //similarly get the parent (if there is one)
      val parentId = for {
        parent <- getElem((n, "parent"))
        _ <- expectAttribute((parent, "type", "guid"))
        parentNodeId <- getNodeText(parent)
      } yield {
        parentNodeId
      }


      UnlinkedAccount(version, id, accountName, accountType, description, parentId.toOption)
    }
  }

  def parseAccountNodes(n: Node): Either[ValidationError, Seq[UnlinkedAccount]] = {
    case class ParseAccountNodesError(override val msg: String) extends ValidationError(msg)
    implicit def errorType: String => ValidationError = ParseAccountNodesError.apply

    getElems((n, "account"))
      //filter out any nodes that don't have the gnc namespace
      .map(_.filter(n => hasNamespace((n, "gnc")).isRight))

      .flatMap { accs =>
      val empty: Either[ValidationError, Seq[UnlinkedAccount]] = Right(Seq())

      //parse all account nodes and fail early if any don't succeed
      accs.foldLeft(empty) {
        //stop parsing the seq
        case (Left(err), thisNode) => Left(err)

        //parse the next item in the seq
        case (Right(acc), thisNode) => parseAccountNode(thisNode) match {
          case Right(a) => Right(acc.+:(a))
          case Left(err) => Left(err)
        }
      }
    }
  }


}

object Parser {

  def linkAccounts(accounts: Seq[UnlinkedAccount]): Either[ValidationError, Seq[LinkedAccount]] = {
    case class LinkAccountsError(override val msg: String) extends ValidationError(msg)
    implicit def errorType: String => ValidationError = LinkAccountsError.apply

    import scala.collection.mutable

    //make a map of GUID -> Account
    val unlinkedAccountsMap: Map[String, UnlinkedAccount] = accounts
      .map(a => (a.id, a))
      .toMap

    val linkedAccountsMap: mutable.Map[String, LinkedAccount] = mutable.Map.empty

    //check that the input only has 1 root account
    val numRoots = accounts.count(_.parentId.isEmpty)
    if(numRoots != 1) {
     Left(errorType(s"There ought to be only 1 root account: ${accounts.map(_.parentId.isEmpty)}"))
    } else {

      //recursively construct the accounts tree by looking up each parent node and caching
      //the results in a Map
      def lookupOrInsert(uacc: UnlinkedAccount): LinkedAccount = {
        uacc.parentId match {
          case Some(id) => {
            linkedAccountsMap.get(id) match {
              //we've cached this value in the map, return it
              case Some(found) => uacc.link(Some(found))
              case None => {
                //recurse to find the parent
                val parentRes = lookupOrInsert(unlinkedAccountsMap
                  .get(id)
                  .getOrElse(
                    throw new LinkAccountsError("lookups should never fail in unlinkedAccountsMap")))

                linkedAccountsMap += ((parentRes.id, parentRes))

                val linkedAccount = uacc.link(Some(parentRes))
                //insert it
                linkedAccountsMap += ((linkedAccount.id, linkedAccount))

                //and return it
                linkedAccount
              }
            }
          }
          //handle the root account specially so it can be looked up in the map
          case None => {
            val linkedRoot = uacc.link(None)
            linkedAccountsMap.put(uacc.id, linkedRoot)
            linkedRoot
          }
        }
      }


      val res = accounts.map(lookupOrInsert)

      //make sure there's only 1 unlinked account
      //i.e. it should be a tree, not a forest
      val rootNodes = res
        .filter(_.parent.isEmpty)
        .toSeq

      if(rootNodes.length == 1) {
        Right(res.toSeq)
      } else if(rootNodes.length == 0) {
        Left(errorType(s"No root nodes found in linkAccounts: $rootNodes"))
      } else {
        Left(errorType("Found >1 root node in linkAccounts (the account tree should be a _tree_, not" +
          s" a forest): $rootNodes"))
      }
    }
  }
}
