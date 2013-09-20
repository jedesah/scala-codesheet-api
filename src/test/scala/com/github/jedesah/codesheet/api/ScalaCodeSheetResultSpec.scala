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
				BlockResult(Nil).userRepr === ""
			}
			"one" in {
				val result = BlockResult(List(
					ExpressionResult("3", line = 1)
				))
				result.userRepr ==== "3"
			}
			"two" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult("5", line = 1)), line = 1),
					ExpressionResult("true", line = 2)
				))
				val expected = """a = 5
								 |true""".stripMargin
				result.userRepr ==== expected
			}
			"three" in {
				val result = BlockResult(List(
					DefDefResult("perform", Nil, None, BlockResult(ExpressionResult("bar", line = 1)), line = 1),
					ValDefResult("other", Some("Animal"), BlockResult(ExpressionResult("Dog(Maven)", line = 2)), line = 2),
					ExpressionResult("1.5", line = 3)
				))
				val expected = """perform => bar
								 |other: Animal = Dog(Maven)
								 |1.5""".stripMargin
				result.userRepr ==== expected
			}
			"on same line" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult(5, line = 1)), line = 1),
					ExpressionResult(true, line = 1)
				))
				val expected = "a = 5; true"
				result.userRepr ==== expected
			}
			"with space" in {
				val result = BlockResult(List(
					ValDefResult("a", None, BlockResult(ExpressionResult(5, line = 1)), line = 1),
					ExpressionResult(true, line = 3)
				))
				val expected = """a = 5
								 |
								 |true""".stripMargin
				result.userRepr ==== expected
			}
		}
		"ValDefResult" in {
			"simple block" in {
				val result = ValDefResult("a", None, BlockResult(ExpressionResult("5", line = 1)), line = 1)
				result.userRepr === "a = 5"
			}
			"with inferred type" in {
				val result = ValDefResult("a", Some("Animal"), BlockResult(ExpressionResult("Cat(5)", line = 1)), line = 1)
				result.userRepr === "a: Animal = Cat(5)"
			}
			"with complex block" in {
				val result = ValDefResult("a", None, BlockResult(List(
					ValDefResult("cc", None, BlockResult(ExpressionResult("Cat(4)", line = 2)), line = 2),
					ExpressionResult("Cat(6)", line = 3)
				)), line = 1)
				val expected = """a = {
								 |	cc = Cat(4)
								 |	Cat(6)
								 |}""".stripMargin
				result.userRepr ==== expected
			}
			"on seperate line (no block)" in {
				val result = ValDefResult("a", None, BlockResult(ExpressionResult(45, line = 2)), line = 1)
				result.userRepr === """a =
									  |	45""".stripMargin
			}
		}
		"DefDefResult" in {
			"simple block" in {
				val result = DefDefResult("perform", Nil, None, BlockResult(ExpressionResult("5", line = 1)), line = 1)
				result.userRepr === "perform => 5"
			}
			"with params" in {
				val params = List(
					AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(5))),
					AssignOrNamedArg(Ident(newTermName("tui")), Literal(Constant(true)))
				)
				val result = DefDefResult("perform", params, None, BlockResult(ExpressionResult("5", line = 1)), line = 1)
				result.userRepr === "perform(a = 5, tui = true) => 5"
			}
			"with inferred type" in {
				val result = DefDefResult("perform", Nil, Some("Any"), BlockResult(ExpressionResult(true, line = 1)), line = 1)
				result.userRepr === "perform: Any => true"
			}
			"with complex block" in {
				val result = DefDefResult("perform", Nil, None, BlockResult(List(
					ValDefResult("cc", None, BlockResult(ExpressionResult("Cat(4)", line = 2)), line = 2),
					ExpressionResult("Cat(6)", line = 3)
				)), line = 1)
				val expected = """perform => {
								 |	cc = Cat(4)
								 |	Cat(6)
								 |}""".stripMargin
				result.userRepr ==== expected
			}
		}
		"ExpressionResult" in {
			"with steps" in {
				val result = ExpressionResult(steps = List(tb.parse("3 + 3")), final_ = 6, line = 1)
				result.userRepr === "3 + 3 => 6"
			}
			"without steps" in {
				val result = ExpressionResult(true, line = 1)
				result.userRepr === "true"
			}
			"Unit" in {
				val result = ExpressionResult(Unit, line = 1)
				result.userRepr === ""
			}
		}
		"ExceptionResult" in {
			val result = ExceptionResult(new IndexOutOfBoundsException())
			result.userRepr === "throws java.lang.IndexOutOfBoundsException"
		}
		"NotImplementedResult" in {
			val result = NotImplementedResult
			result.userRepr === "???"
		}
		"ObjectResult" in {
			val result = ObjectResult(6)
			result.userRepr === "6"
		}
		"CompileErrorResult" in {
			val result = CompileErrorResult("Your code sucks dude", 0)
			result.userRepr === "Your code sucks dude"
		}
		"ClassDefResult" in {
			"no body" in {
				val members = List(AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(5))))
				val result = ClassDefResult("Dog", members, BlockResult(Nil), line = 1)
				result.userRepr === "Dog(a = 5)"
			}
			"with body" in {
				val members = List(
					AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))),
					AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3)))
				)
				val params = List(
					AssignOrNamedArg(Ident(newTermName("mult")), Literal(Constant(3)))
				)
				val innerFun = DefDefResult("humanAge", params, None, BlockResult(List(ExpressionResult(9, line = 2))), line = 2)

				val classDef = ClassDefResult("Cat", members, BlockResult(List(innerFun)), line = 1)
				classDef.userRepr === """Cat(name = "foo", age = 3) {
										|	humanAge(mult = 3) => 9
										|}""".stripMargin
			}
		}
		"ModuleDefResult" in {
			"no body" in {
				val result = ModuleDefResult("Test", BlockResult(Nil), line = 1)
				result.userRepr === ""
			}
			"with body" in {
				val body = BlockResult(List(DefDefResult("singular", Nil, None, BlockResult(List(ExpressionResult(9, line = 2))), line = 2)))
				val result = ModuleDefResult("Test", body, line = 1)
				result.userRepr === """Test {
									  |	singular => 9
									  |}""".stripMargin
			}
		}
	}
}

