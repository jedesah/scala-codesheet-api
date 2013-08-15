package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class AugmentedTreeSpec extends Specification {
	"AugmentedTree" should {
		"isSimpleExpression" should {
			"2 * 3 - 2" in {
				val complexExpression =  "2 * 3 - 2"
				val toolBox = cm.mkToolBox()
				val tree = toolBox.parse(complexExpression)
				tree.isSimpleExpression ==== false
			}
			"""Car("BMW", 5)""" in {
				val simpleExpression =  """Car("BMW", 5)"""
				val toolBox = cm.mkToolBox()
				val tree = toolBox.parse(simpleExpression)
				tree.isSimpleExpression ==== true
			}
			"\"hello\"" in {
				val simpleExpression =  "\"hello\""
				val toolBox = cm.mkToolBox()
				val tree = toolBox.parse(simpleExpression)
				tree.isSimpleExpression ==== true
			}
			"5" in {
				val simpleExpression =  "5"
				val toolBox = cm.mkToolBox()
				val tree = toolBox.parse(simpleExpression)
				tree.isSimpleExpression ==== true
			}
			"""new Car("BMW", 2013)""" in {
				val simpleExpression = """new Car("BMW", 2013)"""
				val toolBox = cm.mkToolBox()
				val tree = toolBox.parse(simpleExpression)
				tree.isSimpleExpression ==== true
			}
		}
		"prettyPrint" in {
			"addition" in {
				val tb = cm.mkToolBox()
				val tree = tb.parse("3 + 3")
				tree.prettyPrint ==== "3 + 3"
			}
			"substraction" in {
				val tb = cm.mkToolBox()
				val tree = tb.parse("3 - 3")
				tree.prettyPrint ==== "3 - 3"
			}
			"comparison" in {
				val tb = cm.mkToolBox()
				val tree = tb.parse("3 > 3")
				tree.prettyPrint ==== "3 > 3"
			}
			"complex" in {
				val tb = cm.mkToolBox()
				val tree = tb.parse("perform() / retrieve(a)")
				tree.prettyPrint ==== "perform() / retrieve(a)"
			}
		}
	}
}