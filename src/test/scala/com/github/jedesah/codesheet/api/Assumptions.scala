package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class Assumptions extends Specification {
	"toolBox.eval()" should {
		"use defined variables outside of actual tree being evaled (in my code)" in {
			val bigUglyVariable = 1
			val toolBox = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
			toolBox.eval(toolBox.parse("com.github.jedesah.codesheet.api.Random.test")) ==== 1
		}
	}
	"toolbox rangepos" should {
		"work" in {
			val tb = cm.mkToolBox(options = "-Yrangepos")
			val code = """val a = {
						 |	val b = 6 + 7
						 |	b * 2
						 |}""".stripMargin
			val AST = tb.parse(code)
			AST.pos.isRange ==== true
		}
	}
}

object Random {
	val test = 1
}