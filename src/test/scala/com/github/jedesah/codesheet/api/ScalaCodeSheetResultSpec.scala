package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

class ScalaCodeSheetResult extends Specification {
	implicit def oneElemToList[T](elem: T) = List(elem)
	"ScalaCodeSheetResult" should {
		"CompositeResult" in {
			"empty" in {
				CompositeResult(Nil).toString === ""
			}
			"one" in {
				val result = CompositeResult(List(
					ExpressionResult(Nil, "3", 0)
				))
				result.toString === "3"
			}
			"two" in {
				val result = CompositeResult(List(
					ValDefResult("a", None, CompositeResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 1)
				))
				val expected = """a = 5
								 |true""".stripMargin
				result.toString ==== expected
			}
			"three" in {
				val result = CompositeResult(List(
					DefDefResult("perform", Nil, None, CompositeResult(ExpressionResult(Nil, "bar", 0)), 0),
					ValDefResult("other", Some("Animal"), CompositeResult(ExpressionResult(Nil, "Dog(Maven)", 0)), 1),
					ExpressionResult(Nil, "1.5", 2)
				))
				val expected = """perform => bar
								 |other: Animal = Dog(Maven)
								 |1.5""".stripMargin
				result.toString ==== expected
			}
			"on same line" in {
				val result = CompositeResult(List(
					ValDefResult("a", None, CompositeResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 0)
				))
				val expected = "a = 5; true"
				result.toString ==== expected
			}
			"with space" in {
				val result = CompositeResult(List(
					ValDefResult("a", None, CompositeResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 2)
				))
				val expected = """a = 5
								 |
								 |true""".stripMargin
				result.toString ==== expected
			}
		}
		"ValDefResult" in {
			pending
		}
		"DefDefResult" in {
			pending
		}
		"ExpressionResult" in {
			pending
		}
		"ExpressionResult" in {
			pending
		}
		"NotImplementedResult" in {
			pending
		}
	}
}

