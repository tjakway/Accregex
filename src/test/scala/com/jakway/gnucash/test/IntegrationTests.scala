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
    copyResourceToFile(TestResources.foodTest),
    copyResourceToFile(TestResources.foodTestRules),
    getTempFile(tempDir).right.get,
    false,
    false,
    false,
    false,
    "Imbalance-USD",
    None,
    true,
    UnvalidatedConfig.default.verbosity.withDebug()
  )

  "The Driver" should "pass the food test" in {
    new Driver(foodTestConf).runEither() shouldEqual Right(null)
  }
}
