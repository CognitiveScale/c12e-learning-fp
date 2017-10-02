import com.c12e.sbt.plugins.StaticChecksPlugin.{Wart, wartsIgnoredIn}

val v_sbtTestInterface = "1.0"
val v_scalacheck = "1.13.5"
val v_scalaz = "7.2.15"
val v_utest = "0.5.3"

libraryDependencies ++=
  Seq("org.scalacheck" %% "scalacheck" % v_scalacheck % Test)

// We might use these later
//
//libraryDependencies ++=
//  Seq(
//    "org.scalaz" %% "scalaz-core" % v_scalaz,
//    "org.scalaz" %% "scalaz-concurrent" % v_scalaz,
//    "org.scala-sbt" % "test-interface" % v_sbtTestInterface % Test,
//    "com.lihaoyi" %% "utest" % v_utest % Test)
//
//testFrameworks += new TestFramework("utest.runner.Framework")

wartsIgnoredIn(Test)(
  Wart.Overloading,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.ImplicitConversion,
  Wart.NonUnitStatements,
  Wart.Var,
  Wart.StringPlusAny,
  Wart.PublicInference)
