import com.c12e.sbt.Build._


libraryDependencies ++=
  Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.8",
    "org.scalaz" %% "scalaz-concurrent" % "7.2.8",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test")
