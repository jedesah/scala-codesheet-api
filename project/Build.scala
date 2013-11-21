import sbt._
import Keys._

import de.johoop.jacoco4sbt._
import JacocoPlugin._
import bintray.Plugin.bintrayPublishSettings

import com.typesafe.sbt.SbtGit.versionWithGit

object ApplicationBuild extends Build {

	lazy val main = Project(
		id = "main",
		base = file("."),
		settings = Project.defaultSettings ++ bintrayPublishSettings ++ jacoco.settings ++ versionWithGit ++ Seq(
			organization := "com.github.jedesah",
			name := "scala-insight",
			scalaVersion := "2.10.3",
			licenses += ("LGPL-3.0", url("http://www.gnu.org/copyleft/lgpl.html")),
			libraryDependencies ++= Seq(
				"org.scala-lang" % "scala-compiler" % scalaVersion.value,
				"com.github.nscala-time" %% "nscala-time" % "0.6.0",
				"org.specs2" %% "specs2" % "2.3.1" % "test"
			),
			initialCommands in console := """|
				|import com.github.jedesah.codesheet.api.ScalaCodeSheet
				|import scala.reflect.runtime.{currentMirror => cm}
				|import scala.reflect.runtime.universe._
				|import scala.tools.reflect.ToolBox
				|val tb = cm.mkToolBox()
			""".stripMargin
			//,scalacOptions ++= Seq("-feature")
		)
	)
}