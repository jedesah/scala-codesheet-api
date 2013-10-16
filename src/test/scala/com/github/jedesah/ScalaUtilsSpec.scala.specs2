package com.github.jedesah

import org.specs2.mutable._

import ScalaUtils._

class ScalaUtilsSpec extends Specification {
	"toSource" should {
		"char" in {
			case class A(c: Char)
			val a = A('c')
			toSource(a) === Some("A('c')")
		}
	}
}