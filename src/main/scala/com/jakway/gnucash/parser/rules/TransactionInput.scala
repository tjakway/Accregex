package com.jakway.gnucash.parser.rules

import com.jakway.gnucash.parser.{LinkedAccount, ValidationError}

case class Transaction(id: String,
                       description: String,
                       splits: Seq[Split]) {

  /**
    * is the passed account a source of funds
    * aka whether the transaction debits from the linked account
    * @param linkedAccount
    * @return
    */
  def isSourceAccount(linkedAccount: LinkedAccount): Boolean = {
    splits.foldLeft(false) {
      case (true, _) => true
      case (false, Split(_, account, value))
        if(account == linkedAccount) =>  {

        //check whether we deduct funds from that account
        if(value < 0) {
          true
        } else {
          false
        }
      }

        //ignore other accounts
      case (acc, _) => acc
    }
  }

  /**
    * for a transaction to be valid all splits must sum to 0
    * @return
    */
  def isValid: Boolean = {
    splits.foldLeft(0: Double) {
      case (sum, Split(_, _, value)) => sum + value
    } == 0
  }
}


/**
  * @param id
  * @param value
  * @param on the account the value of the split modifies
  */
case class Split(id: String,
                 on: LinkedAccount,
                 value: Double)
