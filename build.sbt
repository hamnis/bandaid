name := "pointy"

organization := "net.hamnaberg.json"

scalaVersion := "2.10.1"

crossScalaVersions := Seq("2.9.1", "2.9.1-1", "2.9.2", "2.9.3", "2.10.1")

libraryDependencies += "org.json4s" %% "json4s-ast" % "3.2.2"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.2" % "test"

libraryDependencies <+= (scalaBinaryVersion) { sv =>
  sv.split("\\.").toList match {
    case "2" :: "10" :: Nil => "org.specs2" %% "specs2" % "1.14" % "test"
    case "2" :: "9" :: "3" :: Nil => "org.specs2" %% "specs2" % "1.12.4.1" % "test"
    case "2" :: "9" :: _ :: Nil => "org.specs2" %% "specs2" % "1.12.4" % "test"
    case _ => sys.error("Unsupported scala version: " + sv)
  }
}
