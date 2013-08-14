package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class ScalaCodeSheetResult extends Specification {
	implicit def oneElemToList[T](elem: T) = List(elem)
	val tb = cm.mkToolBox()
	"ScalaCodeSheetResult" should {
		"BlockResult" in {
			"empty" in {
				BlockResult(Nil).toString === ""
			}
			"one" in {
				val result = BlockResult(List(
					ExpressionResult(Nil, "3", 0)
				))
				result.toString === "3"
			}
			"two" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 1)
				))
				val expected = """a = 5
								 |true""".stripMargin
				result.toString ==== expected
			}
			"three" in {
				val result = BlockResult(List(
					DefDefResult("perform", Nil, None, BlockResult(ExpressionResult(Nil, "bar", 0)), 0),
					ValDefResult("other", Some("Animal"), BlockResult(ExpressionResult(Nil, "Dog(Maven)", 0)), 1),
					ExpressionResult(Nil, "1.5", 2)
				))
				val expected = """perform => bar
								 |other: Animal = Dog(Maven)
								 |1.5""".stripMargin
				result.toString ==== expected
			}
			"on same line" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 0)
				))
				val expected = "a = 5; true"
				result.toString ==== expected
			}
			"with space" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult(Nil, "5", 0)), 0),
					ExpressionResult(Nil, "true", 2)
				))
				val expected = """a = 5
								 |
								 |true""".stripMargin
				result.toString ==== expected
			}
		}
		"ValDefResult" in {
			"simple block" in {
				val result = ValDefResult("a", None, BlockResult(ExpressionResult(Nil, "5", 0)), 0)
				result.toString === "a = 5"
			}
			"with inferred type" in {
				val result = ValDefResult("a", Some("Animal"), BlockResult(ExpressionResult(Nil, "Cat(5)", 0)), 0)
				result.toString === "a: Animal = Cat(5)"
			}
			"with complex block" in {
				val result = ValDefResult("a", None, BlockResult(List(
					ValDefResult("cc", None, BlockResult(ExpressionResult(Nil, "Cat(4)", 0)), 1),
					ExpressionResult(Nil, "Cat(6)", 2)
				)), 0)
				val expected = """a = {
								 |	cc = Cat(4)
								 |	Cat(6)
								 |}""".stripMargin
				result.toString ==== expected
			}
		}
		"DefDefResult" in {
			"simple block" in {
				val result = DefDefResult("perform", Nil, None, BlockResult(ExpressionResult(Nil, "5", 0)), 0)
				result.toString === "perform => 5"
			}
			"with params" in {
				val params = List(
					AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(5))),
					AssignOrNamedArg(Ident(newTermName("tui")), Literal(Constant(true)))
				)
				val result = DefDefResult("perform", params, None, BlockResult(ExpressionResult(Nil, "5", 0)), 0)
				result.toString === "perform(a = 5, tui = true) => 5"
			}
			"with inferred type" in {
				val result = DefDefResult("perform", Nil, Some("Any"), BlockResult(ExpressionResult(Nil, "true", 0)), 0)
				result.toString === "perform: Any => true"
			}
			"with complex block" in {
				val result = DefDefResult("perform", Nil, None, BlockResult(List(
					ValDefResult("cc", None, BlockResult(ExpressionResult(Nil, "Cat(4)", 0)), 1),
					ExpressionResult(Nil, "Cat(6)", 2)
				)), 0)
				val expected = """perform => {
								 |	cc = Cat(4)
								 |	Cat(6)
								 |}""".stripMargin
				result.toString ==== expected
			}
		}
		"ExpressionResult" in {
			val result = ExpressionResult(List(tb.parse("3 + 3")), ObjectResult(6), 0)
			result.toString === "3 + 3 => 6"
		}
		"ExceptionResult" in {
			val result = ExceptionResult(new IndexOutOfBoundsException(), 0)
			result.toString === "throws java.lang.IndexOutOfBoundsException"
		}
		"NotImplementedResult" in {
			val result = NotImplementedResult(0)
			result.toString === "???"
		}
		"ObjectResult" in {
			val result = ObjectResult(6, 0)
			result.toString === "6"
		}
	}
}

