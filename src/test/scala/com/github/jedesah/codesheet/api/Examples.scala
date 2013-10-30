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
						body must beLike { case IfThenElseResult(cond, then, else_, 2) =>
							cond ==== SimpleExpressionResult(false, Nil, line = 2)
							then ==== SimpleExpressionResult(Nil, Nil, line = 2)
							else_ must beLike { case BlockResult(List(smaller, larger, expr), 3) =>
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
			/*"2" in {
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
						body ==== SimpleExpressionResult(List(3,5,7), Nil, line = 2)
					}
				}
			}*/
		}
	}
}