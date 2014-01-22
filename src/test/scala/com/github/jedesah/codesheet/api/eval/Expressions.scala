package com.github.jedesah.insight

import org.specs2.mutable._

import Eval._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.Flag._

class Expressions extends Specification {

	val compiler = createDefaultPC

	def structureEquals(first: compiler.Tree, second: compiler.Tree) = 
		if (first.equalsStructure(second)) ok
		else first === second

	def verifyException(ex: Throwable) = ex must beLike { case ex: java.lang.ArithmeticException => ex.getMessage ==== "/ by zero" }

	"Expressions" should {
		"literal" in {
			val originalASTs = List(compiler.reify("1").tree)
			compiler.eval(originalASTs) must beLike { case Result(asts, "") =>
				asts.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				asts(0).attachments.get[SimpleExpressionResult] ==== Some(SimpleExpressionResult(1, Nil))
			}
		}
		"typical" in {
			val originalASTs = parse("1 + 1")
			eval(originalASTs) must beLike { case Result(ASTs, "") =>
				ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				ASTs(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(2, Nil)
			}
		}
		"two lines" in {
			val code = """1 + 1
                         |4 * 4""".stripMargin
            val originalASTs = parse(code)
			eval(originalASTs) must beLike { case Result(ASTs, "") =>
				ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				ASTs(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(2, Nil)
				ASTs(1).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(16, Nil)
			}
		}
		"with empty line" in {
			val code = """1 + 1
                         |
                         |4 * 4""".stripMargin
            val originalASTs = parse(code)
			eval(originalASTs) must beLike { case Result(ASTs, "") =>
				ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				ASTs(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(2, Nil)
				ASTs(1).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(16, Nil)
			}
		}
		"expression returning Unit" in {
			val code = "if (true && false) 34"
			val originalASTs = parse(code)
			eval(originalASTs) must beLike { case Result(ASTs, "") =>
				ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				ASTs(0).children(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(false, Nil)
				ASTs(0).children(2).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult((), Nil)
			}
		}
		"multiline expression" in {
			val code = """if (true || false)
                         |	45 + 23""".stripMargin
            val originalASTs = parse(code)
			eval(originalASTs) must beLike { case Result(ASTs, "") =>
				ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
				ASTs(0).children(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(true, Nil)
				ASTs(0).children(1).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult(68, Nil)
			}
		}
		"empty" in {
			eval("") ==== Result(Nil, "")
		}
		"string interpolation" in {
			"steps" in {
				val code = """ s"allo" """
				val originalASTs = parse(code)
				eval(originalASTs) must beLike { case Result(ASTs, "") =>
					ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
					ASTs(0).children(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult("allo", Nil)
				}
			}
			"no steps" in {
				val code = """ s"allo" """
				val originalASTs = parse(code)
				eval(originalASTs, false) must beLike { case Result(ASTs, "") =>
					ASTs.zip(originalASTs).map{ case (orig, trans) => structureEquals(orig, trans)}
					ASTs(0).children(0).attachements.get[SimpleExpressionResult] ==== SimpleExpressionResult("allo", Nil)
				}
			}
		}
	}
}