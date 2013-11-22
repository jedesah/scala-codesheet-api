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
			val code = """def quickSort(list: List[Int]): List[Int] = {
						 |	if (list.isEmpty) Nil
    					 |	else {
    					 |		val smaller = list.tail.filter(_ < list.head)
    					 |		val larger = list.tail.diff(smaller)
        				 |		quickSort(smaller) ++ (list.head :: quickSort(larger))
    					 |	}
						 |}
						 |
						 |quickSort(List(3,1,2,6,8,9))""".stripMargin
			"returns the right programming structure" in {
				computeResults(code) must beLike { case Result(List(defSort, callSort), "") =>
					defSort must beLike { case DefDefResult("quickSort", List(param), None, body, 1) =>
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
					callSort ==== SimpleExpressionResult(List(1,2,3,6,8,9), Nil, 10)
				}
			}
			"prints correctly to the screen" in {
				computeResults(code).userRepr must not be empty;
			}
			"slighly more compact version" in {
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
			}.pendingUntilFixed("We don't have great support for pattern matching value definitions quite wet, so this one will have to wait a bit")
		}
	}
}