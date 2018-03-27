package com.jakway.gnucash

case class Config(rules: String, summarize: Boolean)

object Config {
  val default: Config = Config("rules.conf", true)
  val progName = "accregex"

  val parser = new scopt.OptionParser[Config](progName) {
    head(progName)

    opt[String]('f', "rules")
      .action((x, c) => c.copy(rules = x))
      .text(s"The file to load transaction rules from (default=${default.rules})")

    opt[Boolean]('s', "summarize")
      .action((x, c) => c.copy(summarize = x))
      .text(s"Whether to print a summary of changes (default=${default.summarize})")

  }
}
