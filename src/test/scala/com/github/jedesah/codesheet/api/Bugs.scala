package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

class Bugs extends Specification {

	"Bugs" should {
		"dev1" in {
			"1" in {
				val code =
				  """val a = 3
					|val b = 'f'
					|val c = true
					|List(a,b,c)
				  """.stripMargin
				computeResults(code, enableSteps = false) must beLike { case StandardResult(List(a, b, c, expr), "") =>
					a ==== ValDefResult("a", None, SimpleExpressionResult(3, Nil, 1), 1)
					b ==== ValDefResult("b", None, SimpleExpressionResult('f', Nil, 2), 2)
					c ==== ValDefResult("c", None, SimpleExpressionResult(true, Nil, 3), 3)
					expr ==== SimpleExpressionResult(List(3, 'f', true), Nil, 4)
				}
			}
			"AnyVal" in {
				val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
				computeResults(code, enableSteps = false) must beLike { case StandardResult(List(foo), "") =>
					foo must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
						ok
					}
				}
			}
		}
		"github issues" in {
			"#23 Position is wrong for object" in {
				// I don't think the bug has to do with the exact nature of the code pasted in the bug report which
				// is why the test result below is based very loosely on the code found in the bug report
				val start = ValDefResult("a", None, SimpleExpressionResult(Map(), Nil, line = 1), line = 1)
				val object_ = ModuleDefResult("A", BlockResult(List(SimpleExpressionResult(true, Nil, line = 4)), line = 3), line = 3)
				val end = SimpleExpressionResult(Map(), Nil, line = 7)
				val result = StandardResult(List(start, object_, end), "")
				"userRepr" in {
					val expected = """a = Map()
									 |
									 |A {
									 |	true
									 |}
									 |
									 |Map()""".stripMargin
					result.userRepr ==== expected
				}
				"computeResults" in {
					val code = """val a = Map()
								 |
								 |object A {
								 |	true
								 |}
								 |
								 |a""".stripMargin
					computeResults(code, false) ==== result
				}
			}
		}
	}
}
