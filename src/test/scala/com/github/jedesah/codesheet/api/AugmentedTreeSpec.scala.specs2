package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class AugmentedTreeSpec extends Specification {
	"AugmentedTree" should {
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