import sbt._
import Keys._

object ScalaCodeSheetAPIBuild extends Build {
  val mySettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.jedesah",
    name         := "codesheet-api",
    version      := "0.3-SNAPSHOT",
    scalaVersion := "2.11.0-M4",
    libraryDependencies := Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      //"org.specs2" %% "specs2" % "2.2.3" % "test"
      "org.scalatest" %% "scalatest" % "2.0.M6-SNAP35" % "test"
    )
  )

  val setupReplClassPath = TaskKey[Unit]("setup-repl-classpath", "Set up the repl server's classpath based on our dependencies.")

  lazy val project = Project (
    "codesheet-api",
    file ("."),
    settings = mySettings ++ Seq(
      setupReplClassPath <<= (dependencyClasspath in Compile) map {cp =>
        val cpStr = cp map { case Attributed(str) => str} mkString(System.getProperty("path.separator"))
        println("Repl will use classpath "+ cpStr)
        System.setProperty("codesheet.class.path", cpStr)
      },
      run in Compile <<= (run in Compile).dependsOn(setupReplClassPath)
    )
  )
}