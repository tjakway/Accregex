#!/usr/bin/env bash

AGENTLIB_HPROF_ARGS="-agentlib:hprof=cpu=samples,heap=all,interval=20,depth=10,verbose=n,doe=y"

EXTRA_CP="/dpoolfs/git/accregex/target/scala-2.12/test-classes:/dpoolfs/git/accregex/target/scala-2.12/classes:/home/thomas/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.2.jar:/home/thomas/.ivy2/cache/ch.qos.logback/logback-classic/jars/logback-classic-1.2.1.jar:/home/thomas/.ivy2/cache/ch.qos.logback/logback-core/jars/logback-core-1.2.1.jar:/home/thomas/.ivy2/cache/org.slf4j/slf4j-api/jars/slf4j-api-1.7.22.jar:/home/thomas/.ivy2/cache/com.github.scopt/scopt_2.12/jars/scopt_2.12-3.5.0.jar:/home/thomas/.ivy2/cache/org.scala-lang.modules/scala-xml_2.12/bundles/scala-xml_2.12-1.0.6.jar:/home/thomas/.ivy2/cache/org.json4s/json4s-native_2.12/jars/json4s-native_2.12-3.5.3.jar:/home/thomas/.ivy2/cache/org.json4s/json4s-core_2.12/jars/json4s-core_2.12-3.5.3.jar:/home/thomas/.ivy2/cache/org.json4s/json4s-ast_2.12/jars/json4s-ast_2.12-3.5.3.jar:/home/thomas/.ivy2/cache/org.json4s/json4s-scalap_2.12/jars/json4s-scalap_2.12-3.5.3.jar:/home/thomas/.ivy2/cache/com.thoughtworks.paranamer/paranamer/bundles/paranamer-2.8.jar:/home/thomas/.ivy2/cache/org.json4s/json4s-jackson_2.12/jars/json4s-jackson_2.12-3.5.3.jar:/home/thomas/.ivy2/cache/com.fasterxml.jackson.core/jackson-databind/bundles/jackson-databind-2.8.4.jar:/home/thomas/.ivy2/cache/com.fasterxml.jackson.core/jackson-annotations/bundles/jackson-annotations-2.8.0.jar:/home/thomas/.ivy2/cache/com.fasterxml.jackson.core/jackson-core/bundles/jackson-core-2.8.4.jar:/home/thomas/.ivy2/cache/org.xmlunit/xmlunit-core/jars/xmlunit-core-2.5.1.jar:/home/thomas/.ivy2/cache/org.scalatest/scalatest_2.12/bundles/scalatest_2.12-3.0.4.jar:/home/thomas/.ivy2/cache/org.scalactic/scalactic_2.12/bundles/scalactic_2.12-3.0.4.jar:/home/thomas/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.12.2.jar:/home/thomas/.ivy2/cache/org.scala-lang.modules/scala-parser-combinators_2.12/bundles/scala-parser-combinators_2.12-1.0.4.jar"

java "$AGENTLIB_HPROF_ARGS" -cp "$EXTRA_CP" org.scalatest.tools.Runner \
    -o -R target/scala-2.12/accregex_2.12-1.0-tests.jar $* 