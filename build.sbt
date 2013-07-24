organization := "com.github.jedesah"

name := "codesheet-api"

version := "0.1"

scalaVersion := "2.10.0"

scalaHome := Some(file("/Users/JR/Projects/scala-latest/build/pack/"))

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.specs2" %% "specs2" % "2.1.1" % "test"

//scalacOptions ++= Seq("-feature")
