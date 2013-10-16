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
				computeResults(code, enableSteps = false) must beLike { case BlockResult(List(a, b, c, expr)) =>
					a ==== ValDefResult("a", None, ExpressionResult(3, Nil, 1), 1)
				  	b ==== ValDefResult("b", None, ExpressionResult('f', Nil, 2), 2)
				  	c ==== ValDefResult("c", None, ExpressionResult(true, Nil, 3), 3)
				  	expr ==== ExpressionResult(List(3, 'f', true), Nil, 4)
				}
			}
			"2" in {
				val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
				computeResults(code, enableSteps = false).children.length ==== 1
			}
			"AnyVal" in {
				val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
				computeResults(code, enableSteps = false) must beLike { case BlockResult(List(foo)) =>
					foo must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
						ok//rhs ==== ExpressionResult("3! f! true!", Nil, 1)
					}
				}
			}
		}
	}
}
