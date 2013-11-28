import de.johoop.jacoco4sbt._
import JacocoPlugin._

organization := "com.github.jedesah"

name := "codesheet-api"

version := "0.6.0-SNAPSHOT"

scalaHome := Some(file("/Volumes/Data/JR/Projects/scala/build/pack/"))

seq(bintrayPublishSettings:_*)

licenses += ("GPL-3.0", url("http://www.gnu.org/copyleft/gpl.html"))

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % scalaVersion.value,
	"com.github.nscala-time" %% "nscala-time" % "0.6.0",
	"org.specs2" %% "specs2" % "2.3.1" % "test",
	"com.chuusai" % "shapeless" % "2.0.0-SNAPSHOT" cross CrossVersion.full changing()
)

jacoco.settings

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

//scalacOptions ++= Seq("-feature")

initialCommands in console := """
import com.github.jedesah.codesheet.api.ScalaCodeSheet
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
val tb = cm.mkToolBox(options = "-Yrangepos")
"""