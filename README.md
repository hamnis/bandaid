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

## Living on the edge

	libraryDependencies += "net.hamnaberg.json" %% "bandaid" % "0.2.0-SNAPSHOT"


# Implementation of JSON Pointer and JSON Patch

This project uses submodules.

Make sure you do

    git submodule init
    git submodule update

before you try to build.


# References

[RFC-6901](https://tools.ietf.org/html/rfc6901)
[RFC-6902](https://tools.ietf.org/html/rfc6902)
