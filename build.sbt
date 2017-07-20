import com.c12e.sbt.dependencies._
import com.c12e.sbt.plugins.StaticChecksPlugin.{Wart, wartsIgnoredIn}


libraryDependencies ++=
  Seq(
    scalazCore,
    scalazConcurrent,
    scalaCheck)


wartsIgnoredIn(Test)(
  Wart.Overloading,
  Wart.ExplicitImplicitTypes,
  Wart.FinalCaseClass,
  Wart.ImplicitConversion,
  Wart.NonUnitStatements,
  Wart.Var,
  Wart.StringPlusAny,
  Wart.PublicInference)
