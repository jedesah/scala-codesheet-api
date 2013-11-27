package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

import scala.reflect.runtime.universe._

class Examples extends Specification {

	def structureEquals(first:Tree, second:Tree) = 
		if (first.equalsStructure(second)) ok
		else first === second

	"Examples" should {
		"quickSort" in {
			"1" in {
				val code = """def quickSort(list: List[Int]): List[Int] = {
							 |	if (list.isEmpty) Nil
	    					 |	else {
	    					 |		val smaller = list.tail.filter(_ < list.head)
	    					 |		val larger = list.tail.diff(smaller)
	        				 |		quickSort(smaller) ++ (list.head :: quickSort(larger))
	    					 |	}
							 |}""".stripMargin
				computeResults(code) must beLike { case Result(List(defDef), "") =>
					defDef must beLike { case DefDefResult("quickSort", List(param), None, body, 1) =>
						structureEquals(param, AssignOrNamedArg(Ident(newTermName("list")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
						body must beLike { case IfThenElseResult(cond, executed, 2) =>
							cond ==== SimpleExpressionResult(false, Nil, line = 2)
							executed must beLike { case BlockResult(List(smaller, larger, expr), 3) =>
								smaller ==== ValDefResult("smaller", None, SimpleExpressionResult(Nil, Nil, line = 4), line = 4)
								larger ==== ValDefResult("larger", None, SimpleExpressionResult(List(5,7), Nil, line = 5), line = 5)
								expr ==== SimpleExpressionResult(List(3,5,7), Nil, line = 6)
							}
						}
					}
				}
			}
			"1.1" in {
				val code = """def quickSort(list: List[Int]): List[Int] = {
							 |	if (list.isEmpty) Nil
	    					 |	else {
	    					 |		val smaller = list.tail.filter(_ < list.head)
	    					 |		val larger = list.tail.diff(smaller)
	        				 |		quickSort(smaller) ++ (list.head :: quickSort(larger))
	    					 |	}
							 |}""".stripMargin
				computeResults(code).userRepr must not be empty;
			}
			"2" in {
				val code = """def quickSort(list: List[Int]): List[Int] = {
							 |	if (list.isEmpty) Nil
	    					 |	else {
	    					 |		val (smaller, larger) = list.tail.partition(_ < list.head)
	        				 |		quickSort(smaller) ++ (list.head :: quickSort(larger))
	    					 |	}
							 |}""".stripMargin
				computeResults(code) must beLike { case Result(List(defDef), "") =>
					defDef must beLike { case DefDefResult("quickSort", List(param), None, body, 1) =>
						structureEquals(param, AssignOrNamedArg(Ident(newTermName("list")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
						body must beLike { case IfThenElseResult(cond, executed, 2) =>
							cond ==== SimpleExpressionResult(false, Nil, line = 2)
							executed must beLike { case BlockResult(List(smaller, larger, expr), 3) =>
								smaller ==== ValDefResult("smaller", None, SimpleExpressionResult(Nil, Nil, line = 4), line = 4)
								larger ==== ValDefResult("larger", None, SimpleExpressionResult(List(5,7), Nil, line = 4), line = 4)
								expr ==== SimpleExpressionResult(List(3,5,7), Nil, line = 5)
							}
						}
					}
				}
			}
		}
		"Poor man's type class" in {
			"Part 1" in {
				"just definition of ===" in {
					val code = """object SimpleEquality {
								 |	implicit class Equal[A](left: A) {
	    						 |		def ===(right: A): Boolean = left == right
								 |	}
								 |}""".stripMargin
					computeResults(code, false) must beLike { case Result(List(object_), "") =>
						object_ ==== ModuleDefResult("SimpleEquality", BlockResult(Nil, 1), 1)
					}
				}
				"definition of === + use of it" in {
					val code = """object SimpleEquality {
								 |	implicit class Equal[A](left: A) {
	    						 |		def ===(right: A): Boolean = left == right
								 |	}
								 |	4 === 4
								 |	'a' === 'b'
								 |}""".stripMargin
					computeResults(code, false) must beLike { case Result(List(object_), "") =>
						object_ must beLike { case ModuleDefResult("SimpleEquality", BlockResult(List(expr1, expr2), 1), 1) =>
							expr1 ==== SimpleExpressionResult(true, Nil, 5)
							expr2 ==== SimpleExpressionResult(false, Nil, 6)
						}
					}
				}
				"same thing but no need for a wrapping object cause we are in codebrew" in {
					val code = """implicit class Equal[A](left: A) {
	    						 |	def ===(right: A): Boolean = left == right
								 |}
								 |4 === 4
								 |'a' === 'b'""".stripMargin
					computeResults(code, false) must beLike { case Result(List(expr1, expr2), "") =>
						expr1 ==== SimpleExpressionResult(true, Nil, 4)
						expr2 ==== SimpleExpressionResult(false, Nil, 5)
					}
				}
				"same thing with inline optimization" in {
					val code = """implicit class Equal[A](val left: A) extends AnyVal {
	    						 |	def ===(right: A): Boolean = left == right
	  							 |}
								 |4 === 4
								 |'a' === 'b'""".stripMargin
					computeResults(code, false) must beLike { case Result(List(expr1, expr2), "") =>
						expr1 ==== SimpleExpressionResult(true, Nil, 4)
						expr2 ==== SimpleExpressionResult(false, Nil, 5)
					}
				}.pendingUntilFixed("This does not work because of limitations on values classes. The toolbox simply does not support values classes so there is nothing I can do about it")
			}
			"Part 2" in {
				"relaxed type constraints" in {
					val code = """implicit class Equal[L](left: L) {
								 |	def ===[R](right: R): Boolean = left == right
								 |}
								 |4 === 4
								 |'a' === 'b'
								 |true === 3.0""".stripMargin
					computeResults(code, false) must beLike { case Result(List(expr1, expr2, expr3), "") =>
						expr1 ==== SimpleExpressionResult(true, Nil, 4)
						expr2 ==== SimpleExpressionResult(false, Nil, 5)
						expr3 ==== SimpleExpressionResult(false, Nil, 6)
					}
				}
			}
		}
	}
}