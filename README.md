#Bandaid

Scala implementation of JSON Patch and JSON Pointer



# Usage

```Scala
import bandaid._

val object = ... //some json structure

val is = ... //get inputstream from somewhere...

val patch = Patch.parse(is)

val patched = patch(object)

```



## Release

	libraryDependencies += "net.hamnaberg.json" %% "bandaid" % "0.1.0"

### Living on the edge

	libraryDependencies += "net.hamnaberg.json" %% "bandaid" % "0.2.0-SNAPSHOT"


