lazy val commonSettings = Seq(
    organization := "org.draegisoft",
    version := "0.1.0",
    scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
    settings(commonSettings: _*).
    settings(
        name := "sonny",
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
        libraryDependencies ++= Seq(
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
            "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
        )
    )
