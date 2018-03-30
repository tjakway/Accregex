package com.jakway.gnucash.parser

import scala.annotation.tailrec

class AccountNameParser(val linkedAccounts: Seq[LinkedAccount],
                        val divider: String = ":") {

  class AccountNameParserError(override val msg: String)
    extends ValidationError(msg)

  case class BadAccountStringError(override val msg: String)
    extends ValidationError(msg)

  case class NoMatchesFoundError(override val msg: String)
    extends ValidationError(msg)

  case class TooManyMatchesFoundError(override val msg: String)
    extends ValidationError(msg)

  case class RootAccountError(override val msg: String, a: LinkedAccount)
    extends ValidationError(msg)

  /**
    * Returns account names from MOST -> LEAST specific
    * NOTE: this is the opposite of how they are written in the application, i.e.
    * Assets:Current Assets:My Banks:Bank of America
    * becomes Seq(Bank of America, My Banks, Current Assets, Assets)
    * @param accountStr
    * @return
    */
  def splitAccountStr(accountStr: String) = accountStr
    .split(divider)
    .filter(_.trim != divider)
    .filter(!_.isEmpty)
    .reverse

  def findReferencedAccount(accountStr: String): Either[ValidationError, LinkedAccount] = {
    val sAccountStr = splitAccountStr(accountStr)

    //TODO: could make other types of matches possible (e.g. case-insensitive, wildcards)
    //filter on name exact match
    def filterCandidates(candidates: Seq[LinkedAccount], name: String): Seq[LinkedAccount] =
      candidates
        .filter(_.name.trim == name.trim)

    //tail, but don't throw an exception if empty
    def tailOrEmpty[A](s: Seq[A]): Seq[A] =
      if(s.isEmpty) {
        Seq()
      } else {
        s.tail
      }

    @tailrec
    def helper(acc: Seq[LinkedAccount])
              (candidates: Seq[LinkedAccount],
               namesRemaining: Seq[String]): Either[ValidationError, LinkedAccount] = {
      val thisName: Option[String] = namesRemaining.headOption

      if(namesRemaining.isEmpty) {
        if(candidates.length == 0) {

          val errMsg = s"No candidates found that match $accountStr: " +
            s"no more names to match and no candidates left"
          Left(NoMatchesFoundError(errMsg))

        } else if(candidates.length > 1) {

          Left(TooManyMatchesFoundError(s"Account string $accountStr was not specific enough." +
            s"  Too many candidates remain: $candidates"))
        } else {

          //filter for accounts that are not parents of any accounts in the list
          val res = acc.map { thisItem =>
            val isParent = acc.foldLeft(false) {
              case (cond, q) => cond || q.parent.filter(_ == thisItem).isDefined
            }

            (thisItem, isParent)
            //traverse the list twice, once to mark items that are parents
            //and a second time to filter them
            //we traverse twice in case the first pass would filter early and
            //cause us to miss items that are parents of items at the front of
            //the list
          }.filter(!_._2)


          //implementation error
          case class AccumulatorError(override val msg: String)
            extends AccountNameParserError(msg)

          if(res.length == 1) {
            Right(res.head._1)
          } else {
            Left(AccumulatorError("Expected accumulator to contain exactly 1 item but got " +
              s"$res"))
          }
        }
      } else {
        val thisName = namesRemaining.head
        val nextNames = tailOrEmpty(namesRemaining)

        val currentMatches = filterCandidates(candidates, thisName)
        val nextCandidates = currentMatches.flatMap(_.parent.toSeq)

        helper(acc ++ currentMatches)(nextCandidates, nextNames)
      }
    }

    //validate inpue
    if(sAccountStr.isEmpty) {
      Left(BadAccountStringError(s"$accountStr does not contain any entries"))
    } else {
      helper(Seq())(linkedAccounts, sAccountStr)
        //make sure we're not returning the root account
        .flatMap { x =>
          if(x.isRootAccount) {
            val errMsg = s"Only the root account matches $accountStr, refusing to return it"
            Left(RootAccountError(errMsg, x))
          } else {
            Right(x)
          }
        }
    }
  }
}
