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

  def findReferencedAccount(accountStr: String): Either[ValidationError, LinkedAccount] = {
    val splitAccountStr = accountStr.split(divider)

    //TODO: could make other types of matches possible (e.g. case-insensitive, wildcards)
    //filter on name exact match
    def filterCandidates(candidates: Seq[LinkedAccount], name: String): Seq[LinkedAccount] =
      candidates.filter(_.name.trim == name.trim)

    //tail, but don't throw an exception if empty
    def tailOrEmpty[A](s: Seq[A]): Seq[A] =
      if(s.isEmpty) {
        Seq()
      } else {
        s.tail
      }

    @tailrec
    def helper(candidates: Seq[LinkedAccount],
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
          Right(candidates.head)
        }
      } else {
        val thisName = namesRemaining.head
        val nextNames = tailOrEmpty(namesRemaining)

        val nextCandidates = filterCandidates(candidates, thisName)

        helper(nextCandidates, nextNames)
      }
    }

    //validate input
    if(splitAccountStr.isEmpty) {
      Left(BadAccountStringError(s"$accountStr does not contain any entries"))
    } else {
      helper(linkedAccounts, splitAccountStr)
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
