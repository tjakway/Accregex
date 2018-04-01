package com.jakway.gnucash.parser.rules

import com.jakway.gnucash.parser.LinkedAccount

case class Transaction(id: String,
                       description: String,
                       splits: Seq[Split])


/**
  * @param id
  * @param value
  * @param on the account the value of the split modifies
  */
case class Split(id: String,
                 on: LinkedAccount,
                 value: Double)
