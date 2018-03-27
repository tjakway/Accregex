package com.jakway.gnucash.rules

import com.jakway.gnucash.Config
import com.jakway.gnucash.parser.ValidationError
import org.json4s.JObject

import scala.util.{Failure, Success, Try}

class TransactionRuleLoaderError(override val msg: String)
  extends ValidationError(msg)

/**
  * @param srcToParse JSON source to parse--DO NOT PASS A FILE
  */
class Loader(val srcToParse: String) {
  implicit def errorType: String => ValidationError = new TransactionRuleLoaderError(_)

  def parsePriority(obj: JObject): Either[ValidationError, Double] = {
    import org.json4s.Formats
    import org.json4s.DefaultFormats
    implicit val formats = DefaultFormats

    (obj \ "priority").extractOpt[String] match {
      //if the user entered a priority...
      case Some(priorityStr) => {
        //try to parse it as a number
        case class PriorityParseException(override val msg: String)
          extends TransactionRuleLoaderError(msg)

        val errMsg = s"Could not parse $priorityStr as a number"

        Try(priorityStr.toDouble) match {
          case Success(a) => Right(a)
          case Failure(ex) => {
            val err = PriorityParseException(errMsg)
            err.initCause(ex)
            Left(err)
          }
        }
      }

      //otherwise use the default priority
      case None => Right(UnlinkedTransactionRule.defaultPriority)
    }
  }

  /**
    * get a key from the map and convert it to the indicated type
    * @param m
    * @param key
    * @tparam K
    * @tparam V
    * @tparam A
    * @return
    */
  def getKey[K, V, A](m: Map[K, V])(key: K): Either[ValidationError, A] = {
    val extractedValue = m.get(key) match {
      case Some(v) => Right(v)
      case None => Left(errorType(s"Could not find key $key"))
    }

    extractedValue.flatMap(v => Try(v.asInstanceOf[A]) match {
      case Success(q) => Right(q)
      case Failure(ex) => {
        Left(errorType(s"Could not convert $extractedValue to an instance of the specified type," +
          s" exception thrown: $ex"))
      }
    })
  }

  def parseRule(obj: JObject): Either[ValidationError, UnlinkedTransactionRule] = {

    def extract[A](key: String) = getKey[String, Any, A](obj.values)(key)

    for {
      pattern <- extract[String]("pattern")
      sourceAccount <- extract[String]("sourceAccount")
      destAccount <- extract[String]("destAccount")
      priority <- parsePriority(obj)
    } yield {
      UnlinkedTransactionRule(pattern, priority.toString, sourceAccount, destAccount)
    }
  }

  def parse: Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] = {
    import org.json4s._

    val jsonOpt = org.json4s.native.JsonMethods.parseOpt(srcToParse)

    type AccumulatorType = Either[ValidationError, Seq[UnlinkedTransactionRule]]
    val empty: Either[ValidationError, Seq[UnlinkedTransactionRule]] =
      Right(Seq())

    def accF(a: AccumulatorType, b: JValue): AccumulatorType = (a, b) match {
      case (Right(acc), obj@JObject(_)) => parseRule(obj) match {
        case Left(r) => Left(r)
        case Right(q) => Right(acc.+:(q))
      }
      case (Left(q), _) => Left(q)
      case (_, x) => Left(errorType(s"Expected object child of $json but got $x")): AccumulatorType
    }

    jsonOpt match {
      case None => {
        case class JsonParseError(override val msg: String)
          extends TransactionRuleLoaderError(msg)

        Left(Seq(JsonParseError(s"Could not parse $srcToParse as JSON")))
      }
      case Some(json) => accumulateEithers(json.children.map(_.children.foldLeft(empty)(accF)))
    }

  }

  /**
    * accumulate Rights into 1 Seq unless an error occurs
    * in which case accumulate all errors
    * @param in
    * @tparam A
    * @tparam B
    * @return
    */
  def accumulateEithers[A, B](in: Seq[Either[A, Seq[B]]]):
    Either[Seq[A], Seq[B]] = {
    val empty: Either[Seq[A], Seq[B]] = Right(Seq())

    def f(accs: Either[Seq[A], Seq[B]], thisElem: Either[A, Seq[B]]):
      Either[Seq[A], Seq[B]] = (accs, thisElem) match {

      case (Left(acc), Left(q)) => Left(acc.+:(q))
      case (_, Left(q)) => Left(Seq(q))
      case (Right(acc), Right(q)) => Right(acc ++ q)
      //ignore Right's on error
      case (Left(r), _) => Left(r)
    }

    in.foldLeft(empty)(f)
  }

}


object Loader {
  //root object is "accregex"
  val jsonRoot = Config.progName

  def loadFromFile(path: String): Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] = {
    case class TransactionLoaderFileError(override val msg: String)
      extends ValidationError(msg)

    new Loader(path).parse
  }

  def loadFromFile(path: java.io.File): Either[Seq[ValidationError], Seq[UnlinkedTransactionRule]] =
    loadFromFile(path.toString)

}
