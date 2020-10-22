name := "Scala-Cats"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++=
  Seq(
    "org.typelevel" %% "cats-core"   % "2.1.0",
    "org.typelevel" %% "cats-effect" % "2.1.4"
  )

scalacOptions ++= Seq(
  "-Xfatal-warnings"
)
