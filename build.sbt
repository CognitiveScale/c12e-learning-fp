import com.c12e.sbt.plugins.StaticChecksPlugin.{Wart, wartsIgnoredIn}


libraryDependencies ++=
  Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.8",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.8",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

wartsIgnoredIn(Test)(
  Wart.Overloading,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.ImplicitConversion,
  Wart.NonUnitStatements,
  Wart.Var,
  Wart.StringPlusAny,
  Wart.PublicInference)
