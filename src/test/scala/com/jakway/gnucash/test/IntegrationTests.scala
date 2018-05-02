package com.jakway.gnucash.test

import com.jakway.gnucash.{Config, UnvalidatedConfig, ValidatedConfig}
import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.io.Driver
import org.scalatest.{FlatSpec, Matchers}

object IntegrationTests {
  case class IntegrationTestError(override val msg: String)
    extends ValidationError(msg)
}
import IntegrationTests._

class IntegrationTests
  extends FlatSpec
    with Matchers
    with ResourceFiles {

  val foodTestConf = ValidatedConfig(
    copyResourceToFile(foodTest),
    copyResourceToFile(foodTestRules),
    getTempFile(tempDir).right.get,
    summarize = false,
    compress = false,
    skipInputValidation = false,
    skipOutputValidation = false,
    "Imbalance-USD",
    None,
    checkDiff = true,
    UnvalidatedConfig.default.verbosity.withDebug(false)
  )

  "The Driver" should "pass the food test" in {
    //see http://www.scalatest.org/user_guide/other_goodies#eitherValues
    new Driver(foodTestConf).runEither() should be ('right)
  }
}
