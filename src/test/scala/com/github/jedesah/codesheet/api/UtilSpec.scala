package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class UtilsSpec extends Specification {
	"paramList" should {
		"Some" in {
			val valDefsString = List("val a = 5", "val b = 10", "val c = 15", "val d = 20")
			val toolBox = cm.mkToolBox()
			val valDefsTree = valDefsString.map(toolBox.parse(_).asInstanceOf[ValDef])
			ScalaCodeSheet.paramList(valDefsTree) ==== "(a = 5, b = 10, c = 15, d = 20)"
		}
		"None" in {
			ScalaCodeSheet.paramList(Nil) ==== ""
		}
	}
}