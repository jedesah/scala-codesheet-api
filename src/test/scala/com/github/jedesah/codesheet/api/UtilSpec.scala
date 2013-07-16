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

	"isSimpleExpression" should {
		"2 * 3 - 2" in {
			val complexExpression =  "2 * 3 - 2"
			val toolBox = cm.mkToolBox()
			val tree = toolBox.parse(complexExpression)
			ScalaCodeSheet.isSimpleExpression(tree) ==== false
		}
		"""Car("BMW", 5)""" in {
			val simpleExpression =  """Car("BMW", 5)"""
			val toolBox = cm.mkToolBox()
			val tree = toolBox.parse(simpleExpression)
			ScalaCodeSheet.isSimpleExpression(tree) ==== true
		}
		"\"hello\"" in {
			val simpleExpression =  "\"hello\""
			val toolBox = cm.mkToolBox()
			val tree = toolBox.parse(simpleExpression)
			ScalaCodeSheet.isSimpleExpression(tree) ==== true
		}
		"5" in {
			val simpleExpression =  "5"
			val toolBox = cm.mkToolBox()
			val tree = toolBox.parse(simpleExpression)
			ScalaCodeSheet.isSimpleExpression(tree) ==== true
		}
		"""new Car("BMW", 2013)""" in {
			val simpleExpression = """new Car("BMW", 2013)"""
			val toolBox = cm.mkToolBox()
			val tree = toolBox.parse(simpleExpression)
			println(tree.getClass)
			ScalaCodeSheet.isSimpleExpression(tree) ==== true
		}
	}
}