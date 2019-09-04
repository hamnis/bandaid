name := "bandaid"

organization := "net.hamnaberg.json"
crossScalaVersions := Seq("2.13.0", "2.12.9")
scalaVersion := crossScalaVersions.value.head

libraryDependencies += "org.json4s" %% "json4s-ast" % "3.6.7"
libraryDependencies += "org.json4s" %% "json4s-core" % "3.6.7"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.7" % "test"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.6.0" % "test"


scalacOptions in Test ++= Seq("-Yrangepos")