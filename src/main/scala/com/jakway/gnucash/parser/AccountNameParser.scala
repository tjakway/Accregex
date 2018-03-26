package com.jakway.gnucash.parser

import scala.annotation.tailrec

class AccountNameParser(val linkedAccounts: Seq[LinkedAccount],
                        val divider: String = ":") {

  def findReferencedAccount: ValidateF[String, LinkedAccount] =
    (accountStr: String, errorType: String => ValidationError) => {
    val accounts = accountStr.split(divider)

    @tailrec
    def lookup(candidates: Seq[LinkedAccount],
               namesRemaining: Seq[String]):
        Either[ValidationError, LinkedAccount]
      = namesRemaining.headOption match {
        case None if candidates.isEmpty =>
          Left(errorType(s"Could not find account $accountStr"))
        case None if candidates.length > 1 =>
          Left(errorType(s"More than 1 account matched the name $accountStr"))
        case None if candidates.length == 1 => Right(candidates.head)

        case Some(name) => {

          lazy val accExists = linkedAccounts.exists(_.name == name)

          //the new candidates are any of the current candidates that
          //have parents with account names matching the name segment
          //we're currently looking at
          val newCandidates =
            candidates.filter(!_.parent.filter(_.name == name).isEmpty)

          if(newCandidates.isEmpty && accExists) {
            Left(errorType(s"Could not find account matching $accountStr: " +
              s"got up to section $name with candidates $candidates"))
          } else {
            lookup(newCandidates, namesRemaining.tail)
          }
        }
      }

    lookup(Seq(), accounts)
  }
}
