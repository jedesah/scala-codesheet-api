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
				BlockResult(Nil, line = 1).userRepr === ""
			}
			"one" in {
				val result = BlockResult(
					List(SimpleExpressionResult("3", line = 1)),
					line = 1
				)
				result.userRepr ==== """{"3"}"""
			}
			"two" in {
				val result = BlockResult(
					List(
						ValDefResult("a", None, SimpleExpressionResult("5", line = 1), line = 1),
						SimpleExpressionResult(true, line = 2)
					),
					line = 1
				)
				val expected = """{a = "5"
								 |	true
								 |}""".stripMargin
				result.userRepr ==== expected
			}
			"three" in {
				val result = BlockResult(
					List(
						DefDefResult("perform", Nil, None, SimpleExpressionResult("bar", line = 1), line = 1),
						ValDefResult("other", Some("Animal"), SimpleExpressionResult("Dog(Maven)", line = 2), line = 2),
						SimpleExpressionResult(1.5, line = 3)
					),
					line = 1
				)
				val expected = """{perform => "bar"
								 |	other: Animal = "Dog(Maven)"
								 |	1.5
								 |}""".stripMargin
				result.userRepr ==== expected
			}
			"on same line" in {
				val result = BlockResult(
					List(
						ValDefResult("a", None, SimpleExpressionResult(5, line = 1), line = 1),
						SimpleExpressionResult(true, line = 1)
					),
					line = 1
				)
				val expected = "{a = 5; true}"
				result.userRepr ==== expected
			}
			"with space" in {
				val result = BlockResult(
					List(
						ValDefResult("a", None, SimpleExpressionResult(5, line = 1), line = 1),
						SimpleExpressionResult(true, line = 3)
					),
					line = 1
				)
				val expected = """{a = 5
								 |
								 |	true
								 |}""".stripMargin
				result.userRepr ==== expected
			}
		}
		"ValDefResult" in {
			"simple block" in {
				val result = ValDefResult("a", None, SimpleExpressionResult(5, line = 1), line = 1)
				result.userRepr === "a = 5"
			}
			"with inferred type" in {
				val result = ValDefResult("a", Some("Animal"), SimpleExpressionResult("Cat(5)", line = 1), line = 1)
				result.userRepr === "a: Animal = \"Cat(5)\""
			}
			"with complex block" in {
				val result = ValDefResult("a", None, BlockResult(
					List(
						ValDefResult("cc", None, SimpleExpressionResult("Cat(4)", line = 2), line = 2),
						SimpleExpressionResult("Cat(6)", line = 3)
					),
					line = 1
				), line = 1)
				val expected = """a = {
								 |	cc = "Cat(4)"
								 |	"Cat(6)"
								 |}""".stripMargin
				result.userRepr ==== expected
			}
			"on seperate line (no block)" in {
				val result = ValDefResult("a", None, SimpleExpressionResult(45, line = 2), line = 1)
				result.userRepr === """a =
									  |	45""".stripMargin
			}
		}
		"DefDefResult" in {
			"simple block" in {
				val result = DefDefResult("perform", Nil, None, SimpleExpressionResult(5, line = 1), line = 1)
				result.userRepr === "perform => 5"
			}
			"with params" in {
				val params = List(
					AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(5))),
					AssignOrNamedArg(Ident(newTermName("tui")), Literal(Constant(true)))
				)
				val result = DefDefResult("perform", params, None, SimpleExpressionResult(5, line = 1), line = 1)
				result.userRepr === "perform(a = 5, tui = true) => 5"
			}
			"with inferred type" in {
				val result = DefDefResult("perform", Nil, Some("Any"), SimpleExpressionResult(true, line = 1), line = 1)
				result.userRepr === "perform: Any => true"
			}
			"with complex block" in {
				val result = DefDefResult("perform", Nil, None, BlockResult(
					List(
						ValDefResult("cc", None, SimpleExpressionResult("Cat(4)", line = 2), line = 2),
						SimpleExpressionResult("Cat(6)", line = 3)
					),
					line = 1
				), line = 1)
				val expected = """perform => {
								 |	cc = "Cat(4)"
								 |	"Cat(6)"
								 |}""".stripMargin
				result.userRepr ==== expected
			}
		}
		"ExpressionResult" in {
			"with steps" in {
				val result = SimpleExpressionResult(steps = List(tb.parse("3 + 3")), final_ = 6, line = 1)
				result.userRepr === "3 + 3 => 6"
			}
			"without steps" in {
				val result = SimpleExpressionResult(true, line = 1)
				result.userRepr === "true"
			}
			"Unit" in {
				val result = SimpleExpressionResult(Unit, line = 1)
				result.userRepr === ""
			}
			"case class" in {
				"one param" in {
					case class Cat(name: String)
					val result = SimpleExpressionResult(final_ = Cat("Jack"), line = 1)
					result.userRepr === "Cat(\"Jack\")"
				}
				"two params" in {
					case class Cat(age: Int, name: String)
					val result = SimpleExpressionResult(final_ = Cat(12, "Jack"), line = 1)
					result.userRepr === "Cat(12, \"Jack\")"
				}
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
				val result = ClassDefResult("Dog", members, BlockResult(Nil, line = 1), line = 1)
				result.userRepr === "class Dog(a = 5)"
			}
			"with body" in {
				val members = List(
					AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))),
					AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3)))
				)
				val params = List(
					AssignOrNamedArg(Ident(newTermName("mult")), Literal(Constant(3)))
				)
				val innerFun = DefDefResult("humanAge", params, None, SimpleExpressionResult(9, line = 2), line = 2)

				val classDef = ClassDefResult("Cat", members, BlockResult(List(innerFun), line = 1), line = 1)
				classDef.userRepr === """class Cat(name = "foo", age = 3) {
										|	humanAge(mult = 3) => 9
										|}""".stripMargin
			}
			"abstract" in {
				"simple" in {
					val code =
					  """abstract class Animal {
						|	val a: Int
						|}
					  """.stripMargin
					val valDef = tb.parse(code).asInstanceOf[ClassDef].impl.body.collect { case valDef: ValDef => valDef}.head
					val implementedMembers = List(
						atPos(valDef.pos)(ValDef(Modifiers(), newTermName("a"), TypeTree(), Literal(Constant(3))))
					)
					val result = AbstractClassDefResult(implementedMembers, "Animal", Nil, BlockResult(Nil, line = 1), line = 1)
					val expected =
					  """abstract class Animal {
						|	val a = 3
						|}""".stripMargin
					valDef.pos.line === 2
					result.userRepr ==== expected
				}
				"mixed" in {
					val code =
					  """abstract class Animal {
						|	val a: Int
						|	def b = a * 2
						|}
					  """.stripMargin
					val valDef = tb.parse(code).asInstanceOf[ClassDef].impl.body.collect { case valDef: ValDef => valDef}.head
					val implementedMembers = List(
					  atPos(valDef.pos)(ValDef(Modifiers(), newTermName("a"), TypeTree(), Literal(Constant(3))))
					)
					val result = AbstractClassDefResult(implementedMembers, "Animal", Nil, BlockResult(DefDefResult("b", Nil, None, SimpleExpressionResult(6, Nil, line = 3), line = 3), line = 1), line = 1)
					val expected =
					  """abstract class Animal {
						|	val a = 3
						|	b => 6
						|}""".stripMargin
					valDef.pos.line === 2
					result.userRepr ==== expected
				}
			}
		}
		"ModuleDefResult" in {
			"no body" in {
				val result = ModuleDefResult("Test", BlockResult(Nil, line = 1), line = 1)
				result.userRepr === ""
			}
			"with body" in {
				val body = BlockResult(List(DefDefResult("singular", Nil, None, SimpleExpressionResult(9, line = 2), line = 2)), line = 1)
				val result = ModuleDefResult("Test", body, line = 1)
				result.userRepr === """Test {
									  |	singular => 9
									  |}""".stripMargin
			}
		}
	}
}

