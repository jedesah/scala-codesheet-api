package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class AugmentedClassDefSpec extends Specification {
	"AugmentedClassDef" should {
		"abstract members" in {
			"value" in {
				"one declaration" in {
					val toolBox = cm.mkToolBox()
					val abstractClassDef = toolBox.parse("abstract class A { val a: Int }").asInstanceOf[ClassDef]
					AugmentedClassDef(abstractClassDef).abstractMembers.length ==== 1
				}
				"some declarations and some definitions" in {
					val toolBox = cm.mkToolBox()
					val abstractClassDef = toolBox.parse("abstract class A { val a: Int = 5; val b = 10; val c: String; val d: Long }").asInstanceOf[ClassDef]
					AugmentedClassDef(abstractClassDef).abstractMembers.length ==== 2
				}
			}
			"function" in {
				"one declaration" in {
					val toolBox = cm.mkToolBox()
					val abstractClassDef = toolBox.parse("abstract class A { def a: Int }").asInstanceOf[ClassDef]
					AugmentedClassDef(abstractClassDef).abstractMembers.length ==== 1
				}
				"some declarations and some definitions" in {
					val toolBox = cm.mkToolBox()
					val abstractClassDef = toolBox.parse("abstract class A { def a: Int = 5; def b = 10; def c: String; def d: Long }").asInstanceOf[ClassDef]
					AugmentedClassDef(abstractClassDef).abstractMembers.length ==== 2
				}
			}
			"value/function" in {
				val toolBox = cm.mkToolBox()
				val abstractClassDef = toolBox.parse("abstract class A { def a: Int = 5; val b: Char; def c: String; val d: Long = 5L }").asInstanceOf[ClassDef]
				AugmentedClassDef(abstractClassDef).abstractMembers.length ==== 2
			}
		}
	}
}