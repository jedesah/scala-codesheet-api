import de.johoop.jacoco4sbt._
import JacocoPlugin._

organization := "com.github.jedesah"

name := "codesheet-api"

version := "0.3-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.3"

libraryDependencies += "org.specs2" %% "specs2" % "2.2.3" % "test"

jacoco.settings

//scalacOptions ++= Seq("-feature")
