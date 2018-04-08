package com.jakway.gnucash.io

import com.jakway.gnucash.ValidatedConfig
import com.jakway.gnucash.parser.{Parser, ValidationError}

class Driver(val config: ValidatedConfig) {
  val parser = new Parser()

  def run(): Unit = ???

  private def loadAccounts = {

  }

  private def loadGnucashXMLFile(): Either[ValidationError, ]
}
