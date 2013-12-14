package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.universe.Flag._

class ScalaCodeSheetSpec extends Specification {

	def structureEquals(first:Tree, second:Tree) = 
		if (first.equalsStructure(second)) ok
		else first === second

	def verifyException(ex: Throwable) = ex must beLike { case ex: java.lang.ArithmeticException => ex.getMessage ==== "/ by zero" }

	val tb = cm.mkToolBox()
	//args.select(ex = "simple definition")
	//args.execute(sequential = true)

	"ScalaCodeSheet" should {
		"expressions" in {
			"literal" in {
				computeResults("1") ==== Result(List(SimpleExpressionResult(1, line = 1)), "")
			}
			"typical" in {
				computeResults("1 + 1") ==== Result(List(SimpleExpressionResult(2, line = 1)), "")
			}
			"two lines" in {
				val code = """1 + 1
                             |4 * 4""".stripMargin
				computeResults(code) ==== Result(List(SimpleExpressionResult(2, line = 1), SimpleExpressionResult(16, line = 2)), "")
			}
			"with empty line" in {
				val code = """1 + 1
                             |
                             |4 * 4""".stripMargin
				computeResults(code) must beLike { case Result(List(line1, line3), "") =>
					line1 ==== SimpleExpressionResult(2, line = 1)
					line3 ==== SimpleExpressionResult(16, line = 3)
				}
			}
			"expression returning Unit" in {
				val code = "if (true && false) 34"
				computeResults(code) must beLike { case Result(List(IfThenElseResult(cond, executed, 1)), "") =>
					cond ==== SimpleExpressionResult(false, Nil, 1)
					executed ==== SimpleExpressionResult((), Nil, 1)
				}
			}
			"multiline expression" in {
				val code = """if (true || false)
                             |	45 + 23""".stripMargin
				computeResults(code) must beLike { case Result(List(IfThenElseResult(cond, executed, 1)), "") =>
					cond ==== SimpleExpressionResult(true, Nil, 1)
					executed ==== SimpleExpressionResult(68, Nil, 2)
				}
			}
			"empty" in {
				computeResults("") ==== Result(Nil, "")
			}
			"string interpolation" in {
				"steps" in {
					computeResults(""" s"allo" """) ==== Result(List(SimpleExpressionResult("allo", line = 1)), "")
				}
				"no steps" in {
					computeResults(""" s"allo" """, enableSteps = false) ==== Result(List(SimpleExpressionResult("allo", line = 1)), "")
				}
			}
		}
		"function definition" in {
			"simple definition" in {
				val code = """def hello = "Hello!" """
				val inner = SimpleExpressionResult("Hello!", line = 1)
				computeResults(code) ==== Result(List(DefDefResult("hello", Nil, None , inner, line = 1)), "")
			}
			"complex definition" in {
				val code = "def foo = 10 * 4 - 2"
				val inner = SimpleExpressionResult(38, line = 1)
				computeResults(code) ==== Result(List(DefDefResult("foo", Nil, None , inner, line = 1)), "")
			}
			"with use" in {
				val code = """def hello = "Hello!"
							|
							| hello""".stripMargin
				val inner = SimpleExpressionResult("Hello!", line = 1 )
				val first = DefDefResult("hello", Nil, None , inner, line = 1)
				val second = SimpleExpressionResult("Hello!", line = 3)
				computeResults(code) ==== Result(List(first, second), "")
			}
			"overlapping scope" in {
				val code = """val a = 4
							 |def tot(a: String) = a * 2""".stripMargin
				computeResults(code) must beLike { case Result(List(first, second), "") =>
					first === ValDefResult("a", None, rhs = SimpleExpressionResult(4, line = 1), line = 1)
					second must beLike { case DefDefResult("tot", params, None, rhs, 2) =>
						rhs ==== SimpleExpressionResult(ObjectResult("foofoo"), Nil, 2)
						params must beLike { case List(param) =>
							structureEquals(param, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
						}
					}
				}
			}
			"with params" in {
				"basic types" in {
					"Int" in {
						val code = "def foo(a: Int, b: Int, c: Int) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(1), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))	
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"String" in {
						val code = "def addExclamation(a: String, b: String, c: String) = a + b * 2 + c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("addExclamation", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult("foobarbarbiz"), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("bar"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant("biz"))))
								}
							}
						}						
					}
					"Float" in {
						val code = "def foo(a: Float, b: Float, c: Float) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(-0.5), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(1.5f))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(2.5f))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(4.5f))))
								}
							}
						}
					}
					"Boolean" in {
						val code = "def foo(a: Boolean, b: Boolean, c: Boolean) = a && b || c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(true), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(true))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(false))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
								}
							}
						}
					}
					"Long" in {
						val code = "def foo(a: Long, b: Long, c: Long) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(1), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"Double" in {
						val code = "def foo(a: Double, b: Double, c: Double) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(-0.5), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(1.5))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(2.5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(4.5))))
								}
							}
						}
					}
					"Byte" in {
						val code = "def foo(a: Byte, b: Byte, c: Byte) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(1), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"Short" in {
						val code = "def foo(a: Short, b: Short, c: Short) = a + b - c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(ObjectResult(1), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"Char" in {
						val code = "def addExclamation(a: Char, b: Char, c: Char) = a + b + c"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("addExclamation", params, None, rhs, 1) =>
								val result = 'a' + 'b' + 'c'
								rhs ==== SimpleExpressionResult(ObjectResult(result), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant('a'))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant('b'))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant('c'))))
								}
							}
						}
					}
					"AnyVal" in {
						val code = "def foo(a: AnyVal, b: AnyVal, c: AnyVal) = List(a,b,c)"
						computeResults(code, enableSteps = false) must beLike { case Result(List(foo), "") =>
							foo must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs ==== SimpleExpressionResult(List(3, 'f', true), Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant('f'))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
								}
							}
						}
					}
					"Any" in {
						val code = """def foo(a: Any, b: Any, c: Any) = s"$a! $b! $c!" """
						computeResults(code, enableSteps =  false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === SimpleExpressionResult("3! foo! true!", Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
								}
							}
						}
					}
					"AnyRef" in {
						val code = """def bar(a: AnyRef, b: AnyRef, c: AnyRef) = s"$a! $b! $c!" """
						computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("bar", params, None, rhs, 1) =>
								rhs === SimpleExpressionResult("foo! List(3, 5, 7)! Some(5)!", Nil, 1)
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Some")), List(Literal(Constant(5))))))
								}
							}
						}
					}
					"List" in {
						val code = "def foo(a: List[Int], b: List[Int], c: List[Int]) = if (c.isEmpty) a.length else b.length"
						computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 1) =>
									cond ==== SimpleExpressionResult(false, Nil, 1)
									executed ====  SimpleExpressionResult(0, Nil, 1)
								}
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("List")), List(Literal(Constant(11))))))
								}
							}
						}
					}
					"Option" in {
						val code = "def foo(a: Option[String], b: Option[String], c: Option[String]) = if(c.isEmpty) b else a"
						computeResults(code) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 1) =>
									cond ==== SimpleExpressionResult(false, Nil, 1)
									executed ==== SimpleExpressionResult(Some("foo"), Nil, 1)
								}
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Some")), List(Literal(Constant("foo"))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("None"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Some")), List(Literal(Constant("bar"))))))
								}
							}
						}
					}
					"Seq" in {
						val code = "def foo(a: Seq[Boolean], b: Seq[Boolean], c: Seq[Boolean]) = if (c.isEmpty) a.length else b.take(1)"
						computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 1) =>
									cond ==== SimpleExpressionResult(false, Nil, 1)
									executed ==== SimpleExpressionResult(Nil, Nil, 1)
								}
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(true)), Literal(Constant(false)), Literal(Constant(true))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(false))))))
								}
							}
						}
					}
					"generic" in {
						"wildcard" in {
							val code = "def foo(a: List[_], b: List[_], c: List[_]) = if (c.nonEmpty) a else b"
							computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
								first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
									rhs must beLike { case SimpleExpressionResult(ObjectResult(List(3, 5, 7)), List(step), 1) =>
										structureEquals(step, tb.parse("if (List(true).nonEmpty) List(3, 5, 7) else List()"))
									}
									params must beLike { case List(a,b,c) =>
										structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
										structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))))
										structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("List")), List(Literal(Constant(true))))))
									}
								}
							}
						}.pendingUntilFixed
						"type parameter" in {
							val code = """def interleave[T](first: List[T], second: List[T]): List[T] =
										 |	if (first == Nil) second
										 |	else if (second == Nil) first
										 |	else first.head :: second.head :: interleave(first.tail, second.tail)
										 |}""".stripMargin
							computeResults(code, false) must beLike { case Result(List(ifThenElse), "") =>
								ifThenElse must beLike { case IfThenElseResult(cond, executed, 2) =>
									cond ==== SimpleExpressionResult(false, Nil, 2)
									executed must beLike { case IfThenElseResult(cond, executed, 3) =>
										cond ==== SimpleExpressionResult(true, Nil, 3)
										executed ==== SimpleExpressionResult(List(3,5,7), Nil, 3)
									}
								}
							}
						}.pendingUntilFixed
					}
				}
				"custom class" in {
					"case" in {
						"one case class definition" in {
							"one param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car) = a.model + "-" + a.year""".stripMargin
								computeResults(code) must beLike { case Result(List(first, second), "") =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs ==== SimpleExpressionResult(ObjectResult("foo-3"), Nil, 2)
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
										}
									}
								}
							}
							"two param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car, b: Car) = a.year - b.year""".stripMargin
								computeResults(code, enableSteps = false) must beLike { case Result(List(first, second), "") =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs === SimpleExpressionResult(-2, Nil, 2)
										params must beLike { case List(a, b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Apply(Ident(newTermName("Car")), List(Literal(Constant(5)),Literal(Constant("bar"))))))
										}
									}
								}
	
							}
						}
						"two case class definitions" in {
							"use of first one" in {
								val code = """case class Car(year: Int, model: String)
                                             |case class Person(name: String, age: Int)
                                             |def isMatch(car: Car) = car.year""".stripMargin
								computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third), "") =>
									first must beLike { case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike { case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
										}
									}
									third must beLike { case DefDefResult("isMatch", params, None, rhs, 3) =>
										rhs ==== SimpleExpressionResult(3, Nil, 3)
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("car")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
										}
									}
								}
							}
							"use of second one" in {
								val code = """case class Car(year: Int, model: String)
                                             |case class Person(name: String, age: Int)
                                             |def isMatch(person: Person) = person.name""".stripMargin
								computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third), "") =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
										}
									}
									third must beLike { case DefDefResult("isMatch", params, None, rhs, 3) =>
										rhs ==== SimpleExpressionResult("foo", Nil, 3)
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("person")), Apply(Ident(newTermName("Person")), List(Literal(Constant("foo")),Literal(Constant(3))))))
										}
									}
								}
							}
						}
						"multiple case class definitions" in {
							"one param occurence" in {
								"case 1" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(doc: Document) = doc.author""".stripMargin
									computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth), "") =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil, 3), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										}
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs ==== SimpleExpressionResult("bar", Nil, 4)
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("doc")), Apply(Ident(newTermName("Document")), List(Literal(Constant("foo")),Literal(Constant("bar"))))))
											}
										}
									}
								}
								"case 2" in {
									val code = """case class Car(year: Int, model: String)
                                                 |case class Person(name: String, age: Int)
                                                 |case class Document(text: String, author: String)
                                                 |def isMatch(person: Person) = person.name""".stripMargin
									computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth), "") =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil, 3), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										}
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs ==== SimpleExpressionResult("foo", Nil, 4)
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("person")), Apply(Ident(newTermName("Person")), List(Literal(Constant("foo")),Literal(Constant(3))))))
											}
										}
									}
								}
								"case 3" in {
									val code = """case class Car(year: Int, model: String)
                                                 |case class Person(name: String, age: Int)
                                                 |case class Document(text: String, author: String)
                                                 |def isMatch(car: Car) = car.year""".stripMargin
									computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth), "") =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil, 3), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										}
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs ==== SimpleExpressionResult(3, Nil, 4)
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("car")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
											}
										}
									}
								}
							}
							"multiple occurences" in {
								val code = """case class Car(year: Int, model: String)
                                             |case class Person(name: String, age: Int)
                                             |case class Document(text: String, author: String)
                                             |def isMatch(doc: Document, peps: Person) = doc.author == peps.name""".stripMargin
									computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth), "") =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil, 3), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										}
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs ==== SimpleExpressionResult(false, Nil, 4)
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("doc")), Apply(Ident(newTermName("Document")), List(Literal(Constant("foo")),Literal(Constant("bar"))))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("peps")), Apply(Ident(newTermName("Person")), List(Literal(Constant("biz")),Literal(Constant(3))))))
											}
										}
									}
							}
						}
						"multiple function definitions" in {
							val code = """case class Car(year: Int, model: String)
                                         |case class Person(name: String, age: Int)
                                         |case class Document(text: String, author: String)
                                         |def isMatch(doc: Document, peps: Person) = doc.author == peps.name
                                         |def barCode(car: Car) = car.model + "-" + car.year""".stripMargin
									computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth, fifth), "") =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
									}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil, 2), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
									} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil, 3), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
									}
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs ==== SimpleExpressionResult(false, Nil, 4)
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("doc")), Apply(Ident(newTermName("Document")), List(Literal(Constant("foo")),Literal(Constant("bar"))))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("peps")), Apply(Ident(newTermName("Person")), List(Literal(Constant("biz")),Literal(Constant(3))))))
											}
										}
										fifth must beLike { case DefDefResult("barCode", params, None, rhs, 5) =>
											rhs ==== SimpleExpressionResult("foo-3", Nil, 5)
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("car")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
											}
										}
									}
							}
					}
					"normal" in {
						"one" in {
							val code = """class Car(val year: Int, val model: String)
                                         |def foo(a: Car) = a.model + "-" + a.year""".stripMargin
								computeResults(code, enableSteps = false) must beLike { case Result(List(first, second), "") =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs ==== SimpleExpressionResult("foo-3", Nil, 2)
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Select(New(Ident(newTypeName("Car"))), nme.CONSTRUCTOR), List(Literal(Constant(3)), Literal(Constant("foo"))))))
										}
									}
								}
						}
						"three" in {
							val code = """class Car(val year: Int, val model: String)
                                         |class Hut(oui: Boolean, tot: String)
                                         |class Fat(var calorie: Int, var heat: Int)
                                         |def foo(a: Car, b: Hut, c: Fat) = a.model + "-" + a.year""".stripMargin
								computeResults(code, enableSteps = false) must beLike { case Result(List(first, second, third, fourth), "") =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike {case ClassDefResult("Hut", params, BlockResult(Nil, 2), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("oui")), Literal(Constant(true))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("tot")), Literal(Constant("foo"))))
										}
									} 
									third must beLike {case ClassDefResult("Fat", params, BlockResult(Nil, 3), 3) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("calorie")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("heat")), Literal(Constant(5))))
										}
									}
									fourth must beLike { case DefDefResult("foo", params, None, rhs, 4) =>
										rhs ==== SimpleExpressionResult("foo-3", Nil, 4)
										params must beLike { case List(a, b, c) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Select(New(Ident(newTypeName("Car"))), nme.CONSTRUCTOR), List(Literal(Constant(3)), Literal(Constant("foo"))))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Apply(Select(New(Ident(newTypeName("Hut"))), nme.CONSTRUCTOR), List(Literal(Constant(true)), Literal(Constant("bar"))))))
											structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Select(New(Ident(newTypeName("Fat"))), nme.CONSTRUCTOR), List(Literal(Constant(5)), Literal(Constant(7))))))
										}
									}
								}
							}
						}
					}	
				"mixed" in {
					val code = """def foo(a: Int, b: String, c: Boolean) = if (c) a else b.length"""
					computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
						first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
							rhs must beLike { case IfThenElseResult(cond, executed, 1) =>
								cond ==== SimpleExpressionResult(true, Nil, 1)
								executed ==== SimpleExpressionResult(3, Nil, 1)
							}
							params must beLike { case List(a,b,c) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
								structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
							}
						}
					}
				}
				"with default Values" in {
					val code = "def foo(a: Int, b: Int = 10, c: Int = 1) = a + b - c"
					computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
						first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
							rhs ==== SimpleExpressionResult(12, Nil, 1)
							params must beLike { case List(a,b,c) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(10))))
								structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(1))))
							}
						}
					}
				}
				"multiline" in {
					"one expression" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
                                     | 	if (c) a else b.length""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 2) =>
									cond ==== SimpleExpressionResult(true, Nil, 2)
									executed ==== SimpleExpressionResult(3, Nil, 2)
								}
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
								}
							}
						}
					}
					"one expression on 4 lines" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
                                     | 	if (c)
                                     |		a
                                     |	else
                                     |		b""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 2) =>
									cond ==== SimpleExpressionResult(true, Nil, 2)
									executed ==== SimpleExpressionResult(3, Nil, 3)
								}
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true))))
								}
							}
						}
					}
					"one complex expression on 4 lines" in {
						val code = """def foo(a: List[Int], b: String, c: String, d: Boolean, e: Boolean) =
                                     | 	if (d && e || a.length == 2)
                                     |		b.take(2) + c.drop(3)
                                     |	else
                                     |		b.take(3) + c.drop(1)""".stripMargin
							computeResults(code, enableSteps = false) must beLike { case Result(List(first), "") =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs must beLike { case IfThenElseResult(cond, executed, 2) =>
									cond ==== SimpleExpressionResult(false, Nil, 2)
									executed ==== SimpleExpressionResult("fooar", Nil, 5)
								}
								params must beLike { case List(a,b,c,d,e) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)),Literal(Constant(5)),Literal(Constant(7))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant("bar"))))
									structureEquals(d, AssignOrNamedArg(Ident(newTermName("d")), Literal(Constant(true))))
									structureEquals(e, AssignOrNamedArg(Ident(newTermName("e")), Literal(Constant(false))))
								}
							}
						}
					}
					"multiple inner value definition" in {
						val code = """def foo(a: List[Int], b: Int) = {
                                     |	val temp = a.take(4)
                                     |	val almostDone = b :: temp
                                     |	almostDone.dropRight(1)
                                     |}""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(defdef), "") =>
							defdef must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								params must beLike { case List(a,b) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)),Literal(Constant(5)),Literal(Constant(7))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(11))))
								}
								rhs === BlockResult(
									List(
										ValDefResult("temp", None, rhs = SimpleExpressionResult(List(3, 5, 7), line = 2), line = 2),
										ValDefResult("almostDone", None, rhs = SimpleExpressionResult(List(11, 3, 5, 7), line = 3), line = 3),
										SimpleExpressionResult(List(11, 3, 5), Nil, 4)
									),
									line = 1
								)
							}
						}
					}
				}
			}
		}
		"value definition" in {
			"simple" in {
				val code = "val a = 34"
				computeResults(code) must beLike { case Result(List(first), "") =>
					first === ValDefResult("a", None, rhs = SimpleExpressionResult(34, line = 1), line = 1)
				}
			}
			"complex" in {
				val code = "val a = 2 * 3 - 2"
				computeResults(code) must beLike { case Result(List(first), "") =>
					first === ValDefResult("a", None, rhs = SimpleExpressionResult(4, line = 1), line = 1)
				}
			}
			"with use" in {
				val code = """val a = 34
                             |a + 10""".stripMargin
				computeResults(code, enableSteps = false) must beLike { case Result(List(first, second), "") =>
					first === ValDefResult("a", None, rhs = SimpleExpressionResult(34, line = 1), line = 1)
					second === SimpleExpressionResult(44, Nil, 2)
				}
			}
			"value is function application" in {
				val code = """def perform(a: Int) = a + 5
                             |val gg = perform(4)""".stripMargin
				computeResults(code, enableSteps = false) must beLike { case Result(List(first, second), "") =>
					first must beLike { case DefDefResult("perform", params, None, rhs, 1) =>
						rhs === SimpleExpressionResult(8, Nil, 1)
						params must beLike { case List(a) => structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
						}
					}
					second === ValDefResult("gg", None, rhs = SimpleExpressionResult(9, line = 2), line = 2)
				}
			}
			"two" in {
				val code = """val a = 34
								|val b = 45""".stripMargin
				computeResults(code) must beLike { case Result(List(first, second), "") =>
					first ==== ValDefResult("a", None, rhs = SimpleExpressionResult(34, line = 1), line = 1)
					second ==== ValDefResult("b", None, rhs = SimpleExpressionResult(45, line = 2), line = 2)
				}
			}
			"pattern matching" in {
				"simple" in {
					val code = "val (a, b) = (4,5)"
					computeResults(code) must beLike { case Result(List(a, b), "") =>
						a ==== ValDefResult("a", None, rhs = SimpleExpressionResult(4, line = 1), line = 1)
						b ==== ValDefResult("b", None, rhs = SimpleExpressionResult(5, line = 1), line = 1)
					}
				}
				"more complex" in {
					val code = "val (a :: b, _, _) = (List(4,5,6), 6.0, true)"
					computeResults(code) must beLike { case Result(List(a, b), "") =>
						a ==== ValDefResult("a", None, rhs = SimpleExpressionResult(4, line = 1), line = 1)
						b ==== ValDefResult("b", None, rhs = SimpleExpressionResult(List(5,6), line = 1), line = 1)
					}
				}
			}
		}
		"variable definition" in {
			"simple" in {
				val code = "var a = 56"
				computeResults(code) must beLike { case Result(List(only), "") =>
					only ==== ValDefResult("a", None, rhs = SimpleExpressionResult(56, line = 1), line = 1)
				}
			}
			"with redefinition" in {
				val code = """var a = 45
							 |a = 47""".stripMargin
				computeResults(code) must beLike { case Result(List(first, second), "") =>
					first ==== ValDefResult("a", None, rhs = SimpleExpressionResult(45, line = 1), line = 1)
					second ==== ValDefResult("a", None, rhs = SimpleExpressionResult(47, line = 2), line = 2)
				}
			}
		}
		"block" in {
			val code = """{
                         |	4 + 4
                         |	5
                         |}""".stripMargin
			computeResults(code) must beLike { case Result(List(block), "") =>
				block ==== BlockResult(List(SimpleExpressionResult(8, line = 2), SimpleExpressionResult(5, line = 3)), line = 1)
			}
		}
		"pattern match" in {
			val code = """List(1,2,3) match {
						 |	case List(1,3, _) => false
						 |	case a :: b :: c :: Nil => true
						 |}""".stripMargin
			computeResults(code) must beLike { case Result(List(patternMatch), "") =>
				patternMatch must beLike { case MatchResult(selector, matchedCase, 1) =>
					selector ==== SimpleExpressionResult(List(1,2,3), Nil, 1)
					matchedCase ==== SimpleExpressionResult(true, Nil, 3)
				}
			}
		}
		"class definition" in {
			"simple" in {
				val code = "class Car(model: String, year: Int)"
				computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				}
			}
			"with instantiation" in {
				val code = """case class Car(model: String, year: Int)
                             |val test = Car("Fiat", 1999)""".stripMargin
				computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, valDef), "") =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				 	valDef must beLike {case ValDefResult("test", None, rhs, 2) =>
						rhs must beLike { case SimpleExpressionResult(ObjectResult(obj), Nil, 2) =>
							obj.toString === "Car(Fiat,1999)"
				 		} 
					}
				}
			}
			"with use" in {
				val code = """case class Car(model: String, year: Int)
                             |val test = Car("Fiat", 1999)
                             |test.model
                             |test.year""".stripMargin
				computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, valDef, expression1, expression2), "") =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil, 1),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				 	valDef must beLike {case ValDefResult("test", None, rhs, 2) =>
						rhs must beLike { case SimpleExpressionResult(ObjectResult(obj), Nil, 2) =>
							obj.toString === "Car(Fiat,1999)"
				 		} 
					}
					expression1 === SimpleExpressionResult("Fiat", Nil, 3)
					expression2 === SimpleExpressionResult(1999, Nil, 4)
				}
			}
			"complex" in {
				"1" in {
					val code = """case class Car(model: String, year: Int) {
                                 | 	def drive = "vroum vroum"
                                 |}
                                 |val a = new Car("BMW", 2013)
                                 |a.drive""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, valDef, expression1), "") =>
					 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
					 		}
					 		body === BlockResult(List(DefDefResult("drive", Nil, None, SimpleExpressionResult("vroum vroum", Nil, 2), 2)), line = 1)
						}
					 	valDef must beLike {case ValDefResult("a", None, rhs, 4) =>
							rhs must beLike { case SimpleExpressionResult(ObjectResult(obj), Nil, 4) =>
								obj.toString === "Car(BMW,2013)"
					 		}
						}
						expression1 === SimpleExpressionResult("vroum vroum", Nil, 5)
					}
				}
				"2" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	def license(seed: Int) = model.take(seed) + year + seed
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						 classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
							}
							body must beLike{ case BlockResult(List(DefDefResult("license", params, None, SimpleExpressionResult(ObjectResult("foo33"), Nil, 2), 2)), 1) =>
								params must beLike { case List(seed) =>
									structureEquals(seed, AssignOrNamedArg(Ident(newTermName("seed")), Literal(Constant(3))))
								}
						 	}
						}
					}
				}
				"3" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	def license = 5
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						 classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
							}
							body === BlockResult(List(DefDefResult("license", Nil, None, SimpleExpressionResult(5, Nil, 2), 2)), line = 1)
						}
					}
				}
				"4" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	val a = 5
                                 |	val b = (a + year).toString + model
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
					 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
					 		}
					 		body === BlockResult(
					 			List(
					 				ValDefResult("a", None, SimpleExpressionResult(5, Nil, 2), 2),
					 				ValDefResult("b", None, SimpleExpressionResult("8foo", Nil, 3), 3)
					 			), line = 1)
						}
					}
				}
				"5" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	val aabb = List(1,2,4,5).take(2)
                                 |	def wtv(a: Int) = aabb.drop(a)
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
					 	classDef must beLike { case ClassDefResult("Car", params, body, 1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
					 		}
					 		body must beLike { case BlockResult(List(valDef, defDef), 1) =>
					 			valDef === ValDefResult("aabb", None, SimpleExpressionResult(List(1, 2), Nil, 2), 2)
					 			defDef must beLike {case DefDefResult("wtv", params, None, body, 3) =>
									params must beLike { case List(a) =>
										structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
							 		}
									body === SimpleExpressionResult(Nil, Nil, 3)
					 			}
					 		}
						}
					}
				}
				"6" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	val a = 5
                                 |	val tretre = "goo"
                                 |	val b = true
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
					 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
					 		}
					 		body === BlockResult(
					 			List(
					 				ValDefResult("a", None, SimpleExpressionResult(5, Nil, 2), 2),
					 				ValDefResult("tretre", None, SimpleExpressionResult("goo", Nil, 3), 3),
					 				ValDefResult("b", None, SimpleExpressionResult(true, Nil, 4), 4)
					 			), line = 1)
						}
					}
				}
				"7" in {
					val code = """case class Car(model: String, year: Int) {
                                 |	val a = model
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
					 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
					 		}
					 		body === BlockResult(
					 			List(
					 				ValDefResult("a", None, SimpleExpressionResult("foo", Nil, 2), 2)
					 			), line = 1)
						}
					}
				}
				"with exception" in {
					"in value definition" in {
						val code = """case class A(b: String) {
									 |	val a = 3 / 0
									 |}""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						 	classDef must beLike { case ClassDefResult("A", params, body,1) =>
								params must beLike { case List(b) =>
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
						 		}
						 		body must beLike { case BlockResult(
						 			List(
						 				ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 2), 2)
						 			), 1) =>
						 			verifyException(ex)
						 		}
							}
						}
					}
					"in expression" in {
						val code = """case class A(b: String) {
									 |	3 / 0
									 |}""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						 	classDef must beLike { case ClassDefResult("A", params, body,1) =>
								params must beLike { case List(b) =>
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))))
						 		}
						 		body must beLike { case BlockResult(
						 			List(
						 				SimpleExpressionResult(ExceptionValue(ex), Nil, 2)
						 			), 1) =>
						 			verifyException(ex)
						 		}
							}
						}
					}
				}
			}
			"abstract" in {
				"one value definition" in {
					val code = """abstract class Pug {
                                 |	val a: Int
                                 |}""".stripMargin
          			computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
            			classDef ==== ClassDefResult("Pug", Nil, BlockResult(Nil, 1), 1)
					}
				}
				"one function definition" in {
					val code = """abstract class Pet {
                                 |	def af(g: Int): Boolean
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						classDef ==== ClassDefResult("Pet", Nil, BlockResult(Nil, 1), 1)
					}
				}
				"one function definition followed by evaluated expression" in {
					val code = """abstract class Pet {
                                 |	def af(g: Int): Boolean
                                 |}
                                 |val a = 5 + 5""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, valDef), "") =>
						classDef ==== ClassDefResult("Pet", Nil, BlockResult(Nil, 1), 1)
						valDef ==== ValDefResult("a", None, SimpleExpressionResult(10, Nil, 4), 4)
					}
				}
				"with concrete members" in {
					val code = """abstract class Car(model: String, year: Int) {
                                 |  val a: Int
                                 |  def b = a + model + year
                                 |}""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef), "") =>
						classDef must beLike { case ClassDefResult("Car", params, BlockResult(List(b), 1), 1) =>
							params must beLike { case List(a, b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
							}
							b === DefDefResult("b", Nil, None, SimpleExpressionResult("3foo3", Nil, line = 3), 3)
						}
					}
				}
			}
		}
		"object definition" in {
			"with only simple values" in {
				val code = """object MySingleton {
							|	val a = 5
							|	val tretre = "goo"
							| }""".stripMargin
				computeResults(code, enableSteps = false) must beLike{ case Result(List(moduleDef), "") =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body === BlockResult(List(
							ValDefResult("a", None, SimpleExpressionResult(5, Nil, 2), 2),
							ValDefResult("tretre", None, SimpleExpressionResult("goo", Nil, 3), 3)
						), line = 1)
					}
				}
			}
			"with only values" in {
				val code = """object MySingleton {
						|	val a = 5
						|	val b = a + 4
						| }""".stripMargin
				computeResults(code, enableSteps = false) must beLike{ case Result(List(moduleDef), "") =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body === BlockResult(List(
							ValDefResult("a", None, SimpleExpressionResult(5, Nil, 2), 2),
							ValDefResult("b", None, SimpleExpressionResult(9, Nil, 3), 3)
						), line = 1)
					}
				}
			}
			"with function definition" in {
				val code = """object MySingleton {
							|	def jade(a: String, b: Int) = a + a
							| }""".stripMargin
				computeResults(code, enableSteps = false) must beLike{ case Result(List(moduleDef), "") =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body must beLike { case BlockResult(List(defDef), 1) =>
							defDef must beLike{ case DefDefResult("jade", params, None, rhs, 2) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(3))))
								}
								rhs === SimpleExpressionResult("foofoo", Nil, 2)
							}
						}
					}
				}
			}
			"with values and definitions" in {
				val code = """object MySingleton {
							|	val aabb = 45 + 12
							|	def blabla(a: Char) = aabb.toString + a
							| }""".stripMargin
				computeResults(code, enableSteps = false) must beLike{ case Result(List(moduleDef), "") =>
					moduleDef must beLike { case ModuleDefResult("MySingleton", body, 1) =>
						body must beLike { case BlockResult(List(valDef, defDef), 1) =>
							valDef === ValDefResult("aabb", None, SimpleExpressionResult(57, Nil, 2), 2)
							defDef must beLike{ case DefDefResult("blabla", params, None, rhs, 3) =>
								params must beLike { case List(a) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant('a'))))
								}
								rhs === SimpleExpressionResult("57a", Nil, 3)
							}
						}
					}
				}
			}
		}
		"sample generation" in {
			"abstract class" in {
				"simple hierarchy" in {
					val code = """abstract class Animal
                                 |case class Cat(a: String) extends Animal
                                 |case class Dog(b: Int) extends Animal
                                 |def gogo(hui: Animal) = hui.toString""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, cat, dog, gogo), "") =>
						classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
						cat must beLike { case ClassDefResult("Cat", List(a), BlockResult(Nil, 2), 2) =>
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
						}
						dog must beLike { case ClassDefResult("Dog", List(b), BlockResult(Nil, 3), 3) =>
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(3))))
						}
						gogo must beLike { case DefDefResult("gogo", List(hui), None, expr, 4) =>
							structureEquals(hui, AssignOrNamedArg(Ident(newTermName("hui")), Apply(Ident(newTermName("Cat")), List(Literal(Constant("foo"))))))
							expr ==== SimpleExpressionResult("Cat(foo)", Nil, 4)
						}
					}
				}

				"no concrete class" in {
					"simple" in {
						val code = """abstract class Animal
                                     |def gogogo(lui: Animal) = 5 + 5""".stripMargin
						computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, gogogo), "") =>
							classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
							gogogo must beLike { case DefDefResult("gogogo", List(hui), None, expr, 2) =>
								structureEquals(hui, AssignOrNamedArg(Ident(newTermName("lui")),
									Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName("Animal"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))
								))
								expr ==== SimpleExpressionResult(10, Nil, 2)
							}
						}
					}
					"with abstract members" in {
						"one value definition" in {
							val code = """abstract class Animal {
                                         |	val y: Int
                                         |}
                                         |def gogogi(yui: Animal) = yui.y + 45""".stripMargin

							computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, gogogi), "") =>
								classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 4) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")),
										Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName("Animal"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), ValDef(Modifiers(), newTermName("y"), TypeTree(), Literal(Constant(3))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))))
									expr ==== SimpleExpressionResult(48, Nil, 4)
								}
							}
						}
						"two value definitions" in {
							val code = """abstract class Animal {
                                         |	val y: Int
                                         |	val z: String
                                         |}
                                         |def gogogi(yui: Animal) = yui.z + (yui.y + 45)""".stripMargin
							computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, gogogi), "") =>
								classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 5) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")),
										Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName("Animal"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), ValDef(Modifiers(), newTermName("y"), TypeTree(), Literal(Constant(3))), ValDef(Modifiers(), newTermName("z"), TypeTree(), Literal(Constant("foo"))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))))
									expr ==== SimpleExpressionResult("foo48", Nil, 5)
								}
							}
						}
						"one function definition" in {
							val code = """abstract class Animal {
                                         |	def y(a: Int): Int
                                         |}
                                         |def gogogi(yui: Animal) = yui.y(3)""".stripMargin
							computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, gogogi), "") =>
								classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 4) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")),
										Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName("Animal"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))), DefDef(Modifiers(), newTermName("y"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("a"), Ident(newTypeName("Int")), EmptyTree))), TypeTree(), Literal(Constant(3))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))))
									expr ==== SimpleExpressionResult(3, Nil, 4)
								}
							}
						}
						"two function definitions" in {
							val code = """abstract class Animal {
                                         |	def y(a: Int): Int
                                         |	def z(b: Char): Boolean
                                         |}
                                         |def gogogi(yui: Animal) = if (yui.z('a')) yui.y(3) else 7""".stripMargin
							computeResults(code, enableSteps = false) must beLike { case Result(List(classDef, gogogi), "") =>
								classDef ==== ClassDefResult("Animal", Nil, BlockResult(Nil, 1),1)
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 5) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")), tb.parse("new Animal { def y(a: Int) = 3; def z(b: Char) = true }")))
									expr must beLike { case IfThenElseResult(cond, executed, 5) =>
										cond ==== SimpleExpressionResult(true, Nil, 5)
										executed ==== SimpleExpressionResult(3, Nil, 5)
									}
								}
							}
						}
					}
				}
			}
			"trait" in {
				"simple hierarchy" in {
					val code = """trait Animal
                                 |case class Cat(a: Int) extends Animal
                                 |case class Dog(b: Boolean) extends Animal
                                 |def tti(bb: Animal) = bb.toString""".stripMargin
					computeResults(code, enableSteps = false) must beLike { case Result(List(cat, dog, tti), "") =>
						cat must beLike { case ClassDefResult("Cat", List(a), BlockResult(Nil, 2), 2) =>
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
						}
						dog must beLike { case ClassDefResult("Dog", List(b), BlockResult(Nil, 3), 3) =>
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(true))))
						}
						tti must beLike { case DefDefResult("tti", List(bb), None, expr, 4) =>
							structureEquals(bb, AssignOrNamedArg(Ident(newTermName("bb")), tb.parse("Cat(3)")))
							expr ==== SimpleExpressionResult("Cat(3)", Nil, 4)
						}
					}
				}
				"no concrete class" in {
					"simple" in {
						val code = """trait Animal
                                     |def tto(bb: Animal) = 5 + 4""".stripMargin
						computeResults(code, false) must beLike { case Result(List(tto), "") =>
							tto must beLike { case DefDefResult("tto", List(bb), None, expr, 2) =>
								structureEquals(bb, AssignOrNamedArg(Ident(newTermName("bb")),
								  Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName("Animal"))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))))))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List()))
								))
								expr ==== SimpleExpressionResult(9, Nil, 2)
							}
						}
					}
					"with abstract members" in {
						"one value definition" in {
							val code = """trait Animal {
                                         |	val y: Int
                                         |}
                                         |def gogogi(yui: Animal) = yui.y + 45""".stripMargin
							computeResults(code, false) must beLike { case Result(List(gogogi), "") =>
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 4) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")), tb.parse("new Animal { val y = 3}")))
									expr ==== SimpleExpressionResult(48, Nil, 4)
								}
							}
						}
						"two value definitions" in {
							val code = """trait Animal {
                                         |	val y: Int
                                         |	val z: String
                                         |}
                                         |def gogogi(yui: Animal) = yui.z + (yui.y + 45)""".stripMargin
							computeResults(code, false) must beLike { case Result(List(gogogi), "") =>
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 5) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")), tb.parse("""new Animal { val y = 3; val z = "foo"}""")))
									expr ==== SimpleExpressionResult("foo48", Nil, 5)
								}
							}
						}
						"one function definition" in {
							val code = """trait Animal {
                                         |	def y(a: Int): Int
                                         |}
                                         |def gogogi(yui: Animal) = yui.y(3)""".stripMargin
							computeResults(code, false) must beLike { case Result(List(gogogi), "") =>
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 4) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")), tb.parse("new Animal { def y(a: Int) = 3}")))
									expr ==== SimpleExpressionResult(3, Nil, 4)
								}
							}
						}
						"two function definitions" in {
							val code = """trait Animal {
                                         |	def y(a: Int): Int
                                         |	def z(b: Char): Boolean
                                         |}
                                         |def gogogi(yui: Animal) = if (yui.z('a')) yui.y(3) else 7""".stripMargin
							computeResults(code, false) must beLike { case Result(List(gogogi), "") =>
								gogogi must beLike { case DefDefResult("gogogi", List(yui), None, expr, 5) =>
									structureEquals(yui, AssignOrNamedArg(Ident(newTermName("yui")), tb.parse("new Animal { def y(a: Int) = 3; def z(b: Char) = true }")))
									expr must beLike { case IfThenElseResult(cond, executed, 5) =>
										cond ==== SimpleExpressionResult(true, Nil, 5)
										executed ==== SimpleExpressionResult(3, Nil, 5)
									}
								}
							}
						}
					}
				}
			}
		}
		"???" in {
			"simple definitions" in {
				"value definition" in {
					val code = "val a = ???"
					computeResults(code, false) must beLike { case Result(List(ValDefResult("a", None, rhs, 1)), "") =>
						rhs ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
					}
				}
				"function definition" in {
					"no params" in {
						val code = "def gog = ???"
						computeResults(code, false) must beLike { case Result(List(DefDefResult("gog", Nil, None, rhs, 1)), "") =>
							rhs ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
						}
					}
					"with params" in {
						val code = "def gogg(a: Int) = ???"
						computeResults(code, false) must beLike { case Result(List(DefDefResult("gogg", params, None, rhs, 1)), "") =>
							params must beLike { case List(param) =>
								structureEquals(param, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
							}
							rhs ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
						}
					}
				}
			}
			"complex scenarios" in {
				"unimplemented value definition with later use" in {
					"1" in {
						val code = """val a: Int = ???
									 |val b = a""".stripMargin
						computeResults(code, false) must beLike { case Result(List(a, b), "") =>
							a ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
							b ==== SimpleExpressionResult(NotImplementedResult, Nil, 2)
						}
					}.pendingUntilFixed("Need to wait for change to what a Non-Fatal error is in Scala 2.11")
					"2" in {
						val code = """val a: Int = ???
									 |val b = a * 2""".stripMargin
						computeResults(code, false) must beLike { case Result(List(a, b), "") =>
							a ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
							b ==== SimpleExpressionResult(NotImplementedResult, Nil, 2)
						}
					}.pendingUntilFixed("Need to wait for change to what a Non-Fatal error is in Scala 2.11")
				}
				"unimplemented function definition with later use" in {
					val code = """def a: Int = ???
								 |val b = a * 2""".stripMargin
					computeResults(code, false) must beLike { case Result(List(a, b), "") =>
						a ==== SimpleExpressionResult(NotImplementedResult, Nil, 1)
						b ==== SimpleExpressionResult(NotImplementedResult, Nil, 2)
					}
				}.pendingUntilFixed("Need to wait for change to what a Non-Fatal error is in Scala 2.11")
			}
		}
		"throws an exception" in {
			"value definition" in {
				val code = "val a = 10 / 0"
				computeResults(code, false) must beLike { case Result(List(a), "") =>
					a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
						verifyException(ex)
					}
				}
			}
			"function definition" in {
				val code = "def a = 10 / 0"
				computeResults(code, false) must beLike { case Result(List(a), "") =>
					a must beLike { case DefDefResult("a", Nil, None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
						verifyException(ex)
					}
				}
			}
			"value definition followed by" in {
				"an unrelated expression" in {
					val code = """val a = 10 / 0
								 |7 * 7""".stripMargin
					computeResults(code, false) must beLike { case Result(List(a, expr), "") =>
						a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
							verifyException(ex)
						}
						expr ==== SimpleExpressionResult(49, Nil, 2)
					}
				}
				"a related expression" in {
					val code = """val a = 10 / 0
								 |a * 2""".stripMargin
					computeResults(code, false) must beLike { case Result(List(a, expr), "") =>
						a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
							verifyException(ex)
						}
						expr must beLike { case SimpleExpressionResult(ExceptionValue(ex), Nil, 2) =>
							verifyException(ex)
						}
					}
				}
			}
			"top level" in {
				"no output" in {
					"throws an exception" in {
						val code = "var a = 10 / 0"
						computeResults(code, false) must beLike { case Result(List(a), "") =>
							a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
								verifyException(ex)
							}
						}
					}
				}
				"output" in {
					val message = "JeanRemi works too hard"
					val code = s"""print("$message")
								 |var a = 10 / 0""".stripMargin
					computeResults(code, false) must beLike { case Result(List(print, a), actualMessage) =>
						print ==== SimpleExpressionResult((), Nil, 1)
						a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 2), 2) =>
							verifyException(ex)
						}
						actualMessage ==== message
					}
				}
				"followed by a related expression" in {
					val code = """var a = 10 / 0
								 |a * 10""".stripMargin
					computeResults(code, false) must beLike { case Result(List(a), "") =>
						a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 1), 1) =>
							verifyException(ex)
						}
					}
				}
			}
			"inside block" in {
				val code = """{
							 |	val a = 3 / 0
							 |	val b = a * 2
							 |	val c = b / 3
							 |}
							 |4 * 4""".stripMargin
				computeResults(code, false) must beLike { case Result(List(block, last), "") =>
					block must beLike { case BlockResult(List(a), 1) =>
						a must beLike { case ValDefResult("a", None, SimpleExpressionResult(ExceptionValue(ex), Nil, 2), 2) =>
							verifyException(ex)
						}
					}
					last ==== SimpleExpressionResult(16, Nil, 6)
				}
			}
		}
		"import" in {
			"simple" in {
				val code = """import scala.util.Random
				             |if (Random.nextBoolean) 10 else 10""".stripMargin
				computeResults(code, false) must beLike { case Result(List(IfThenElseResult(_, executed, 2)), "") =>
					executed ==== SimpleExpressionResult(10, Nil, 2)
				}
			}
			"relative" in {
				val code = """import scala._
				             |import util.Random
				             |if (Random.nextBoolean) 10 else 10""".stripMargin
				computeResults(code, false) ==== Result(List(SimpleExpressionResult(10, Nil, 3)), "")
			}.pendingUntilFixed("There is a bug in the reflection compiler. It does not support relative imports. See: https://issues.scala-lang.org/browse/SI-6393")
		}
		"mutability" in {
			"println" in {
				val code = """object A {
							 |	def print = println("Hello World!")
							 |}
							 |A.print""".stripMargin
				computeResults(code, false) must beLike { case Result(List(module, expr), "Hello World!") =>
					module must beLike { case ModuleDefResult("A", block, 1) =>
						block must beLike { case BlockResult(List(printDef), 1) =>
							printDef ==== DefDefResult("print", Nil, None, SimpleExpressionResult((), Nil, 2), line = 2)
						}
					}
					expr ==== SimpleExpressionResult((), Nil, line = 4)
				}
			}.pendingUntilFixed("The current instrumentation strategy does not support mutability. A special case can be made for ouput statements, but solving the general problem of mutability does not appear to be easy")
			"variable" in {
				val code = """var a = 5
							 |class B {
							 |	def do = { a = a + 1; a}
							 |}
							 |a""".stripMargin
				computeResults(code, false) must beLike { case Result(List(_, _, expr), "") =>
					expr ==== SimpleExpressionResult(5, Nil, line = 5)
				}
			}.pendingUntilFixed("The current instrumentation strategy does not support mutability as explained above")
		}
		"ouput" in {
			val code = """println("Yo!")
						 |3 + 3
						 |println("Hello World!")""".stripMargin
			computeResults(code, false) must beLike { case Result(List(print1, expr, print2), output) =>
				print1 ==== SimpleExpressionResult((), Nil, line = 1)
				expr ==== SimpleExpressionResult(6, Nil, line = 2)
				print2 ==== SimpleExpressionResult((), Nil, line = 3)
				output ==== "Yo!\nHello World!\n"
			}
		}
		"dependencies" in {
			val code = """import com.github.nscala_time.time.Imports._
						 |DateTime.nextMonth > DateTime.now""".stripMargin
			computeResults(code, false) must beLike { case Result(List(expr), "") =>
				expr ==== SimpleExpressionResult(true, Nil, line = 2)
			}
		}
	}
}