import de.johoop.jacoco4sbt._
import JacocoPlugin._

organization := "com.github.jedesah"

name := "codesheet-api"

versionWithGit

scalaVersion := "2.10.3"

seq(bintrayPublishSettings:_*)

licenses += ("GPL-3.0", url("http://www.gnu.org/copyleft/gpl.html"))

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.specs2" %% "specs2" % "2.2.3" % "test"

jacoco.settings

//scalacOptions ++= Seq("-feature")

initialCommands in console := """
import com.github.jedesah.codesheet.api.ScalaCodeSheet
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
val tb = cm.mkToolBox()
"""