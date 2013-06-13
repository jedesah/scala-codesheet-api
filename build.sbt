scalaVersion := "2.10.0"

scalaHome := Some(file("/Users/JR/Projects/scala/build/pack/"))

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.specs2" %% "specs2" % "2.0-RC1" % "test"

parallelExecution in Test := false