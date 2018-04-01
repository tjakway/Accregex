package com.jakway.gnucash.parser.rules

import com.jakway.gnucash.parser.{LinkedAccount, ValidationError}


/**
  * @param id
  * @param value
  * @param on the account the value of the split modifies
  */
case class Split(id: String,
                 on: LinkedAccount,
                 value: Double)

case class Transaction(id: String,
                       description: String,
                       splits: Seq[Split]) {

  private def cmpAccount(cmp: Double => Boolean)(linkedAccount: LinkedAccount): Boolean = {
    splits.foldLeft(false) {
      case (true, _) => true
      case (false, Split(_, account, value))
        if(account == linkedAccount) =>  {

        //check the sign of value
        cmp(value)
      }

        //ignore other accounts
      case (acc, _) => acc
    }
  }

  /**
    * is the passed account a source of funds
    * aka whether the transaction debits from the linked account
    * @return
    */
  def isSourceAccount: LinkedAccount => Boolean = cmpAccount(isDebitor)


  /**
    * is the passed account given funds by this transaction
    * aka whether the transaction credits to the linked account
    * @return
    */
  def isDestAccount: LinkedAccount => Boolean = cmpAccount(isCreditor)

  private def isDebitor: Double => Boolean =
    value =>
      if(value < 0) {
        true
      } else {
        false
      }

  //TODO: not sure if 0 should be a creditor or debtor
  private def isCreditor: Double => Boolean =
    value =>
      if(value >= 0) {
        true
      } else {
        false
      }

  private def splitsSumZero: Boolean = {
    splits.foldLeft(0: Double) {
      case (sum, Split(_, _, value)) => sum + value
    } == 0
  }

  private def noRootAccount: Boolean = {
    //no transaction should touch the root account directly
    splits.foldLeft(false) {
      case (acc, Split(_, on, _)) => acc || on.parent.isEmpty
    }
  }

  /**
    * for a transaction to be valid all splits must sum to 0
    * @return
    */
  def isValid: Boolean = splitsSumZero && noRootAccount
}


