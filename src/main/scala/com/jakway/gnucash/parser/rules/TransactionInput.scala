package com.jakway.gnucash.parser.rules

import com.jakway.gnucash.parser.LinkedAccount

case class TransactionInput(description: String,
                            sourceAccount: LinkedAccount,
                            destAccount: LinkedAccount)

