package com.github.jedesah.codesheet.api

import org.scalatest.FunSpec

import scala.tools.reflect.ToolBox
import scala.tools.nsc.interpreter._
import scala.tools.nsc._

class Assumptions extends FunSpec {
  	describe("interpreter") {
		it("not throw an exception") {
			val cmd = new CommandLine(Nil, println)
			import cmd.settings
			settings.classpath.value = System.getProperty("codesheet.class.path")
			val inter = new IMain(settings)
			inter.eval("5 + 10") === 15
		}
  	}
}