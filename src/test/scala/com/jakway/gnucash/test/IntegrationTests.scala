package com.jakway.gnucash.test

import com.jakway.gnucash.{Config, UnvalidatedConfig, ValidatedConfig}
import com.jakway.gnucash.error.ValidationError
import com.jakway.gnucash.io.Driver
import org.scalatest.{FlatSpec, Matchers}

object IntegrationTests {
  case class IntegrationTestError(override val msg: String)
    extends ValidationError(msg)


  trait HasFoodTestConf extends ResourceFiles {
    val foodTestConf =  ValidatedConfig(
      copyResourceToFile(foodTest),
      copyResourceToFile(foodTestRules),
      getTempFile(tempDir).right.get,
      compress = false,
      validateInput = true,
      validateOutput = true,
      "Imbalance-USD",
      None,
      checkDiff = true,
      UnvalidatedConfig.default.verbosity.withDebug(false)
    )
  }
}
import IntegrationTests._

class IntegrationTests
  extends FlatSpec
    with Matchers
    with ResourceFiles
    with HasFoodTestConf {


  def runFoodTest(testName: String, conf: ValidatedConfig = foodTestConf) = {
    it should testName in {
      new Driver(conf).runEither() should be ('right)
    }
  }

  "The Driver" should "pass the food test" in {
    //see http://www.scalatest.org/user_guide/other_goodies#eitherValues
    new Driver(foodTestConf).runEither() should be ('right)
  }

  runFoodTest("pass the food test when skipping input validation",
    foodTestConf.copy(validateInput = false))


  runFoodTest("pass the food test when skipping output validation",
    foodTestConf.copy(validateOutput = false))


  runFoodTest("pass the food test when skipping checkDiff",
    foodTestConf.copy(checkDiff = false))


  //TODO: need to call run() instead of runEither()
  runFoodTest("pass the food test when outputting a summary",
    foodTestConf.copy(verbosity = foodTestConf.verbosity.withPrintSummary(true)))
}
