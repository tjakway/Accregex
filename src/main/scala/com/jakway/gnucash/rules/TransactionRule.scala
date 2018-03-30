package com.jakway.gnucash.rules

import com.jakway.gnucash.parser.LinkedAccount

import scala.util.matching.Regex



case class LinkedTransactionRule(pattern: Regex,
                                 priority: Double,
                                 sourceAccount: LinkedAccount,
                                 destAccount: LinkedAccount)


