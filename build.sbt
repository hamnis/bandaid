name := "bandaid"

organization := "net.hamnaberg.json"

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.10.2", "2.11.0")

libraryDependencies += "org.json4s" %% "json4s-ast" % "3.2.9"

libraryDependencies += "org.json4s" %% "json4s-core" % "3.2.9"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" % "test"

libraryDependencies += "org.specs2" %% "specs2" % "2.3.11" % "test"
