package com.jakway.gnucash.parser

import com.jakway.gnucash.Config
import com.jakway.gnucash.error.{ValidateF, ValidationError}
import com.jakway.gnucash.parser.rules.{Split, Transaction}
import com.jakway.gnucash.parser.xml.NodeTests

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node}

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
                   parent: Option[LinkedAccount]) {
  def isRootAccount: Boolean = parent.isEmpty

  lazy val fullName: String = {
    def helper(accum: List[String], current: LinkedAccount): List[String] = {
      val nextAccum = {
        //don't include the root account in the full name string
        if(current.parent.isDefined) {
          current.name :: accum
        } else {
          accum
        }
      }

      //recurse up until there are no more parents to add
      current.parent match {
        case Some(parent) => helper(nextAccum, parent)
        case None => nextAccum
      }
    }

    helper(List(), this)
      //join the names together
      .reduceLeft(_ + Config.accountNameSeparator + _)
  }
}

class Parser {
  import com.jakway.gnucash.parser.xml.NodeTests._

  def findBookNode: ValidateF[Node, Node] =
    (root, errorType: String => ValidationError) => {
      findOnlyOne((Parser.isBookNode _, root))(errorType)
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
  import com.jakway.gnucash.parser.xml.NodeTests._

  val guidPattern: Regex = """\w{32}""".r

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


  /**
    * @param s
    * @return the split id and the account id
    */
  def parseSplit(s: Node): Either[ValidationError, (String, String, Double)] = {
    case class ParseSplitError(override val msg: String)
      extends ValidationError(msg)
    implicit def errorType: String => ValidationError = ParseSplitError.apply

    for {
      _ <- hasNamespace((s, "trn"))

      idNode <- getElem((s, "id"))
      _ <- hasNamespace((idNode, "split"))
      _ <- expectAttribute((idNode, "type", "guid"))
      id <- getNodeText(idNode)

      accountNode <- getElem((s, "account"))
      _ <- hasNamespace((accountNode, "split"))
      _ <- expectAttribute((accountNode, "type", "guid"))
      accountId <- getNodeText(accountNode)

      valueNode <- getElem((s, "value"))
      _ <- hasNamespace((valueNode, "split"))
      valueStr <- getNodeText(valueNode)

      value <- FractionParser.parseFraction(valueStr)
    } yield {
      (id, accountId, value)
    }
  }

  def isSplit(s: Node): Boolean = parseSplit(s).isRight

  /**
    * @param accounts a map of account id -> account
    * @param n
    * @return
    */
  def parseTransaction(accounts: Map[String, LinkedAccount])(n: Node):
    Either[ValidationError, Transaction] = {

    case class ParseTransactionError(override val msg: String)
      extends ValidationError(msg)

    implicit def errorType: String => ValidationError = ParseTransactionError.apply

    def lookupAccount(id: String): Either[ValidationError, LinkedAccount] = {
      accounts.get(id) match {
        case Some(a) => Right(a)
        case None => Left(errorType(s"Could not find account with id $id in " +
          s"map $accounts"))
      }
    }

    val parsedTransactionData: Either[ValidationError,
                             (String, String, Seq[(String, String, Double)])] =
      //parsing the transaction node
      //TODO: extract date
      for {
      _ <- hasNamespace((n, "gnc"))

      //check & extract the transaction id
      //make sure to exclude the ids of splits
      idNode <- queryElem((n, { x =>
        x.label == "id" &&
        hasNamespace((x, "trn")).isRight &&
        expectAttribute((x, "type", "guid")).isRight
      }))
      _ <- hasNamespace((idNode, "trn"))
      _ <- expectAttribute((idNode, "type", "guid"))
      id <- getNodeText(idNode)

      //check & extract the description (memo)
      descNode <- getElem((n, "description"))
      _ <- hasNamespace((descNode, "trn"))
      description <- getNodeText(descNode)

      splitsRoot <- getElem((n, "splits"))
      _ <- hasNamespace((splitsRoot, "trn"))

      splitNodes <- getElems((splitsRoot, "split"))
      parsedSplits <- ValidationError.accumulateAndWrap(splitNodes.map(parseSplit))
    } yield {
        (id, description, parsedSplits)
    }

    parsedTransactionData.flatMap {
      case (transactionId, description, splitData) => {
        for {
          splits <- ValidationError.accumulateAndWrap(splitData.map {
            case (splitId, accountId, value) => lookupAccount(accountId)
              .map(l => Split(splitId, l, value))
          })
        } yield {
          Transaction(transactionId, description, splits.toSet)
        }
      }
    }
  }

  def isTransaction(accounts: Map[String, LinkedAccount])(n: Node): Boolean =
    parseTransaction(accounts)(n).isRight

  //TODO: refactor to reduce duplication between isSplits and parseTransaction
  def isSplits(n: Node): Boolean = {
    implicit def errorType: String => ValidationError =
      (m: String) => new ValidationError(m)
    val res = for {
      splitsRoot <- getElem((n, "splits"))
      _ <- hasNamespace((splitsRoot, "trn"))
    } yield {splitsRoot}

    res.isRight
  }

  def getSplitAccount: ValidateF[Node, String] =
    (n: Node, et: String => ValidationError) => {
      implicit def errorType: String => ValidationError = et

      for {
        _ <- hasNamespace((n, "split"))
        _ <- expectAttribute((n, "type", "guid"))
        childNode <- onlyOne[Node](n.child)

        guidMatches = guidPattern.findAllIn(childNode.text)

        //make sure it's a guid
        _ <- if(guidMatches.length == 1) {
          Right({})
        }
        else {
          Left(errorType(s"Expected an id node but ${childNode.text} " +
            s"didn't match the guid regex pattern exactly once (numMatches = " +
            s"${guidMatches.length}"))
        }
      } yield {
        childNode.text
      }
  }

  def isSplitAccount(n: Node): Boolean =
    getSplitAccount(n)(errorType = (m: String) => new ValidationError(m))
    .isRight


  private def opTransactionNodes(op: Map[String, LinkedAccount] => Node => Boolean):
    ValidateF[(Node, Map[String, LinkedAccount]), Seq[Node]] =
    (t: (Node, Map[String, LinkedAccount]), errorType: String => ValidationError) => {
      val (bookNode, accountMap) = t
      NodeTests.queryElems((bookNode, op(accountMap)))(errorType)
    }


  def getTransactionNodes: ValidateF[(Node, Map[String, LinkedAccount]), Seq[Node]] =
    opTransactionNodes(isTransaction)

  def filterAllTransactionNodes: ValidateF[(Node, Map[String, LinkedAccount]), Seq[Node]] = {
    def isNotTransaction(x: Map[String, LinkedAccount])(y: Node) = !(isTransaction(x)(y))

    opTransactionNodes(isNotTransaction)
  }

  //both of these filters *should* give the same result
  def filterSingleLevelTransactionNodes(accountMap: Map[String, LinkedAccount])(n: Node): Seq[Node] =
      n.child.filter(!isTransaction(accountMap)(_))

  def replaceTransactionNodes(accountMap: Map[String, LinkedAccount])
                             (book: Node, replacedTransactions: Seq[Node]): Either[ValidationError, Elem] = {
    case class ReplaceTransactionNodesError(override val msg: String)
      extends ValidationError(msg)

    //validate input
    def bookAsElem: Either[ValidationError, Elem] =
      Try(book.asInstanceOf[Elem]) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(ReplaceTransactionNodesError(s"Could not cast book node `$book` to " +
          s"an instance of Elem").withCause(t))
      }

    val preFilterLength = book.child.length

    bookAsElem.flatMap { (bookElem: Elem) =>

      val eitherFilteredChildren = {
        //TODO: test & refactor
        val f1 = filterSingleLevelTransactionNodes(accountMap)(bookElem)
        val f2 = filterAllTransactionNodes((bookElem, accountMap))(ReplaceTransactionNodesError.apply _)
        if(Right(f1) != f2) {
          Left(ReplaceTransactionNodesError(
            s"Expected filter functions to give identical results " +
              s"but got f1=$f1, f2=$f2"))
        } else {
          Right(f1)
        }
      }

      val res = for {
        filteredChildren <- eitherFilteredChildren
      } yield {
        val postFilterLength = filteredChildren.length
        val newChildren = filteredChildren ++ replacedTransactions

        bookElem.copy(child=newChildren)
      }

      res match {
        case Right(repElem)
          if repElem.child.length != preFilterLength => {
          Left(ReplaceTransactionNodesError(s"Expected the same number of " +
            s"replacement nodes as the original tree but " +
            s"repElem.child.length != preFilterLength, where " +
            s"repElem.child.length=${repElem.child.length}, preFilterLength=$preFilterLength," +
            s" and repElem=${repElem}"))
        }
        case Right(repElem) => Right(repElem)
        case Left(x) => Left(x)
      }
    }
  }

  def replaceBookNode(root: Node)(replacementBookNode: Node) = {
    case class ReplaceBookNodeError(override val msg: String)
      extends ValidationError(msg)

    //validate input
    def rootAsElem: Either[ValidationError, Elem] =
      Try(root.asInstanceOf[Elem]) match {
        case Success(x) => Right(x)
        case Failure(t) => Left(ReplaceBookNodeError(s"Could not cast root node `$root` to " +
          s"an instance of Elem").withCause(t))
      }

    //sanity check
    if(!isBookNode(replacementBookNode)) {
      Left(ReplaceBookNodeError("replacementBookNode argument is not a book node" +
        s", replacementBookNode=$replacementBookNode"))
    } else {
      //filter out non-books then append our replacement node
      rootAsElem.flatMap { (rootElem: Elem) =>
        val numBookNodes = rootElem.child.count(isBookNode)

        if(numBookNodes != 1) {
          Left(ReplaceBookNodeError(s"Expected only 1 book node in root element `$root` but " +
            s"numBookNodes=$numBookNodes"))
        } else {
          val filteredChildren = rootElem.child.filter(!isBookNode(_))
          val newChildren = filteredChildren ++ Seq(replacementBookNode)

          Right(rootElem.copy(child=newChildren))
        }
      }
    }
  }

  def isBookNode(n: Node): Boolean = {
    def f = new ValidationError(_)

    n.label == "book" &&
      getAttribute((n, "version"))(f).isRight &&
      hasNamespace((n, "gnc"))(f).isRight
  }
}
