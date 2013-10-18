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
				computeResults(code, enableSteps = false) must beLike { case List(a, b, c, expr) =>
					a ==== ValDefResult("a", None, SimpleExpressionResult(3, Nil, 1), 1)
					b ==== ValDefResult("b", None, SimpleExpressionResult('f', Nil, 2), 2)
					c ==== ValDefResult("c", None, SimpleExpressionResult(true, Nil, 3), 3)
					expr ==== SimpleExpressionResult(List(3, 'f', true), Nil, 4)
				}
			}
			"2" in {
				val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
				computeResults(code, enableSteps = false).size ==== 1
			}
			"AnyVal" in {
				val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
				computeResults(code, enableSteps = false) must beLike { case List(foo) =>
					foo must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
						ok//rhs ==== ExpressionResult("3! f! true!", Nil, 1)
					}
				}
			}
		}
	}
}
