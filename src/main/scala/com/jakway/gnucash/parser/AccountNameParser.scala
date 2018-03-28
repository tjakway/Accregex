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

          val currentCandidates = linkedAccounts.filter(_.name == name)
          val accExists = !linkedAccounts.isEmpty

          //the new candidates are any of the current candidates that
          //have parents with account names matching the name segment
          //we're currently looking at
          lazy val newCandidates =
            candidates.filter(!_.parent.filter(_.name == name).isEmpty)

          val numRemainingNames = namesRemaining.length - 1

          lazy val errMsg = s"Could not find account matching $accountStr: " +
                s"got up to section $name with candidates $candidates"

          //check if this is the last candidate
          if(numRemainingNames == 0) {
            //if so, stop recursing: nowhere left to go

            //did we find it?
            if(currentCandidates.length == 0) {
              Left(errorType(errMsg + " (numNamesRemaining == 0)"))
            } else if(currentCandidates.length > 1) {
              Left(errorType(errMsg + " (too many candidates left--expected exactly 1)"))
            } else {
              //found it
              Right(candidates.head)
            }
          }
          else if(newCandidates.isEmpty && accExists) {
              Left(errorType(errMsg))
          } else {
            lookup(newCandidates, namesRemaining.tail)
          }
        }
      }

    lookup(Seq(), accounts)
  }
}
