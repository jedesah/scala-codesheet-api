import de.johoop.jacoco4sbt._
import JacocoPlugin._

organization := "com.github.jedesah"

name := "codesheet-api"

version := "0.6.0-SNAPSHOT"

scalaVersion := "2.10.4-20131126-231426-da7395016c"

seq(bintrayPublishSettings:_*)

seq(bintrayResolverSettings:_*)

licenses += ("GPL-3.0", url("http://www.gnu.org/copyleft/gpl.html"))

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-compiler" % scalaVersion.value,
	"com.github.nscala-time" % "nscala-time_2.10" % "0.6.0",
	"com.chuusai" % "shapeless_2.10.3" % "2.0.0-SNAPSHOT" changing(),
	"org.specs2" % "specs2_2.10" % "2.3.6" % "test"
)

jacoco.settings

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  bintray.Opts.resolver.repo("jedesah", "maven")
)

ApplicationClasspath.newSettings

//scalacOptions ++= Seq("-feature")

initialCommands in console := """
import com.github.jedesah.insight._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
val tb = cm.mkToolBox()
"""