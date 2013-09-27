package com.github.jedesah.codesheet.api

import org.specs2.mutable._

import ScalaCodeSheet._

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

class ScalaCodeSheetSpec extends Specification {

	def structureEquals(first:Tree, second:Tree) = 
		if (first.equalsStructure(second)) ok
		else first === second

   val tb = cm.mkToolBox()


	"ScalaCodeSheet" should {
		"expressions" in {
			"literal" in {
				computeResults("1") ==== BlockResult(List(ExpressionResult(1, line = 1)))
			}
			"typical" in {
				computeResults("1 + 1") must beLike { case BlockResult(List(ExpressionResult(ObjectResult(2), List(step), 1))) =>
          structureEquals(step, tb.parse("1 + 1"))
        }

			}
			"two lines" in {
				val code = """1 + 1
							| 4 * 4""".stripMargin
				computeResults(code) ==== BlockResult(List(ExpressionResult(2, line = 1), ExpressionResult(16, line = 2)))
			}
			"with empty line" in {
				val code = """1 + 1
					|
					| 4 * 4""".stripMargin
				var expected = BlockResult(List(
					ExpressionResult(2, line = 1),
					ExpressionResult(16, line = 3)
				))
				computeResults(code) ==== expected
			}
			"expression returning Unit" in {
				val code = "if (true && false) 34"
				computeResults(code) ==== BlockResult(List(ExpressionResult((), line = 1)))
			}
			"multiline expression" in {
				val code = """if (true || false)
							|	45 + 23""".stripMargin
				computeResults(code) ==== BlockResult(List(ExpressionResult(68, line = 1)))
			}
			"empty" in {
				computeResults("") ==== BlockResult(Nil)
			}
		}

		"function definition" in {
			"simple definition" in {
				val code = """def hello = "Hello!" """
				val inner = BlockResult(List(ExpressionResult("Hello!", line = 1)))
				computeResults(code) ==== BlockResult(List(DefDefResult("hello", Nil, None , inner, line = 1)))
			}
			"complex definition" in {
				val code = "def foo = 10 * 4 - 2"
				val inner = BlockResult(List(ExpressionResult(38, line = 1)))
				computeResults(code) ==== BlockResult(List(DefDefResult("foo", Nil, None , inner, line = 1)))
			}
			"with use" in {
				val code = """def hello = "Hello!"
							|
							| hello""".stripMargin
				val inner = BlockResult(List(ExpressionResult("Hello!", line = 1 )))
				val first = DefDefResult("hello", Nil, None , inner, line = 1)
				val second = ExpressionResult("Hello!", line = 3)
				computeResults(code) ==== BlockResult(List(first, second))
			}
			"overlapping scope" in {
				val code = """val a = 4
							 |def tot(a: String) = a * 2""".stripMargin
				computeResults(code) must beLike { case BlockResult(List(first, second)) =>
					first === ValDefResult("a", None, rhs = BlockResult(List(ExpressionResult(4, line = 1))), line = 1)
					second must beLike { case DefDefResult("tot", params, None, rhs, 2) =>
						rhs === BlockResult(List(ExpressionResult("foofoo", Nil, 2)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(1, Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))	
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"String" in {
						val code = """def addExclamation(a: String, b: String, c: String) = s"$a! $b! $c!" """
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("addExclamation", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("foo! bar! biz!", Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(-0.5, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(true, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) => {
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(1, Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
						}
					}
					"Double" in {
						val code = "def foo(a: Double, b: Double, c: Double) = a + b - c"
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(-0.5, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(1, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(1, Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7))))
								}
							}
						}
					}
					"Char" in {
						val code = """def addExclamation(a: Char, b: Char, c: Char) = s"$a! $b! $c!" """
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("addExclamation", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("a! b! c!", Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant('a'))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant('b'))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant('c'))))
								}
							}
						}						
					}
					"AnyVal" in {
						val code = """def foo(a: AnyVal, b: AnyVal, c: AnyVal) = s"$a! $b! $c!" """
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("3! f! true!", Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("3! foo! true!", Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("bar", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("foo! List(3, 5, 7)! Some(5)!", Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(0, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(Some("foo"), Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(List(), Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(true)), Literal(Constant(false)), Literal(Constant(true))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(false))))))
								}
							}
						}
					}
					"wildCardGeneric" in {
						val code = "def foo(a: List[_], b: List[_], c: List[_]) = if (c.nonEmpty) a else b"
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(List(3, 5, 7), Nil, 1)))
								params must beLike { case List(a,b,c) =>
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))))
									structureEquals(c, AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("List")), List(Literal(Constant(true))))))
								}
							}
						}						
					}
				}
				"custom class" in {
					"case" in {
						"one case class definition" in {
							"one param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car) = a.model + "-" + a.year""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs === BlockResult(List(ExpressionResult("foo-3", Nil, 2)))
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
										}
									}
								}
	
							}
							"two param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car, b: Car) = a.year - b.year""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs === BlockResult(List(ExpressionResult(-2, Nil, 2)))
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
									  | case class Person(name: String, age: Int)
									  | def isMatch(car: Car) = car.year""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second, third)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
										}
									} 								 
									third must beLike { case DefDefResult("isMatch", params, None, rhs, 3) =>
										rhs === BlockResult(List(ExpressionResult(3, Nil, 3)))
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("car")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
										}
									}
								}
							}
							"use of second one" in {
								val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | def isMatch(person: Person) = person.name""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second, third)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
										}
									} 								 
									third must beLike { case DefDefResult("isMatch", params, None, rhs, 3) =>
										rhs === BlockResult(List(ExpressionResult("foo", Nil, 3)))
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
									computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth)) =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										} 																	 
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs === BlockResult(List(ExpressionResult("bar", Nil, 4)))
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("doc")), Apply(Ident(newTermName("Document")), List(Literal(Constant("foo")),Literal(Constant("bar"))))))
											}
										}
									}
								}
								"case 2" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(person: Person) = person.name""".stripMargin
									computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth)) =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										} 																	 
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs === BlockResult(List(ExpressionResult("foo", Nil, 4)))
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("person")), Apply(Ident(newTermName("Person")), List(Literal(Constant("foo")),Literal(Constant(3))))))
											}
										}
									}
								}
								"case 3" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(car: Car) = car.year""".stripMargin
									computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth)) =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										} 																	 
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs === BlockResult(List(ExpressionResult(3, Nil, 4)))
											params must beLike { case List(a) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("car")), Apply(Ident(newTermName("Car")), List(Literal(Constant(3)),Literal(Constant("foo"))))))
											}
										}
									}
								}
							}
							"multiple occurences" in {
								val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | case class Document(text: String, author: String)
									  | def isMatch(doc: Document, peps: Person) = doc.author == peps.name""".stripMargin
									computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth)) =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
										}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
										} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
										} 																	 
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs === BlockResult(List(ExpressionResult(false, Nil, 4)))
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
									  | case class Person(name: String, age: Int)
									  | case class Document(text: String, author: String)
									  | def isMatch(doc: Document, peps: Person) = doc.author == peps.name
									  | def barCode(car: Car) = car.model + "-" + car.year""".stripMargin
									computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth, fifth)) =>
										first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
											}
									}
										second must beLike {case ClassDefResult("Person", params, BlockResult(Nil), 2) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("name")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("age")), Literal(Constant(3))))
											}
									} 
										third must beLike {case ClassDefResult("Document", params, BlockResult(Nil), 3) =>
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("text")), Literal(Constant("foo"))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("author")), Literal(Constant("bar"))))
											}
									} 																	 
										fourth must beLike { case DefDefResult("isMatch", params, None, rhs, 4) =>
											rhs === BlockResult(List(ExpressionResult(false, Nil, 4)))
											params must beLike { case List(a,b) =>
												structureEquals(a, AssignOrNamedArg(Ident(newTermName("doc")), Apply(Ident(newTermName("Document")), List(Literal(Constant("foo")),Literal(Constant("bar"))))))
												structureEquals(b, AssignOrNamedArg(Ident(newTermName("peps")), Apply(Ident(newTermName("Person")), List(Literal(Constant("biz")),Literal(Constant(3))))))
											}
										}
										fifth must beLike { case DefDefResult("barCode", params, None, rhs, 5) =>
											rhs === BlockResult(List(ExpressionResult("foo-3", Nil, 5)))
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
										| def foo(a: Car) = a.model + "-" + a.year""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									} 
									second must beLike { case DefDefResult("foo", params, None, rhs, 2) =>
										rhs === BlockResult(List(ExpressionResult("foo-3", Nil, 2)))
										params must beLike { case List(a) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Select(New(Ident(newTypeName("Car"))), nme.CONSTRUCTOR), List(Literal(Constant(3)), Literal(Constant("foo"))))))
										}
									}
								}	
						}
						"three" in {
							val code = """class Car(val year: Int, val model: String)
										| class Hut(oui: Boolean, tot: String)
										| class Fat(var calorie: Int, var heat: Int)
										| def foo(a: Car, b: Hut, c: Fat) = a.model + "-" + a.year""".stripMargin
								computeResults(code) must beLike { case BlockResult(List(first, second, third, fourth)) =>
									first must beLike {case ClassDefResult("Car", params, BlockResult(Nil), 1) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
										}
									}
									second must beLike {case ClassDefResult("Hut", params, BlockResult(Nil), 2) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("oui")), Literal(Constant(true))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("tot")), Literal(Constant("foo"))))
										}
									} 
									third must beLike {case ClassDefResult("Fat", params, BlockResult(Nil), 3) =>
										params must beLike { case List(a,b) =>
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("calorie")), Literal(Constant(3))))
											structureEquals(b, AssignOrNamedArg(Ident(newTermName("heat")), Literal(Constant(5))))
										}
									} 																
									fourth must beLike { case DefDefResult("foo", params, None, rhs, 4) =>
										rhs === BlockResult(List(ExpressionResult("foo-3", Nil, 4)))
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
					computeResults(code) must beLike { case BlockResult(List(first)) =>
						first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
							rhs === BlockResult(List(ExpressionResult(3, Nil, 1)))
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
					computeResults(code) must beLike { case BlockResult(List(first)) =>
						first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
							rhs === BlockResult(List(ExpressionResult(12, Nil, 1)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(3, Nil, 2)))
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
						computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult(3, Nil, 2)))
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
							computeResults(code) must beLike { case BlockResult(List(first)) =>
							first must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
								rhs === BlockResult(List(ExpressionResult("fooar", Nil, 2)))
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
									| }""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(defdef)) =>
						defdef must beLike { case DefDefResult("foo", params, None, rhs, 1) =>
							params must beLike { case List(a,b) =>
								structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)),Literal(Constant(5)),Literal(Constant(7))))))
								structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(11))))
							}
							rhs === BlockResult(List(
										ValDefResult("temp", None, rhs = BlockResult(List(ExpressionResult(List(3, 5, 7), line = 2))), line = 2),
										ValDefResult("almostDone", None, rhs = BlockResult(List(ExpressionResult(List(11, 3, 5, 7), line = 3))), line = 3),
										ExpressionResult(List(11, 3, 5), Nil, 4)	
							))
							}										
						}
					}
				}
			}	
		}
		
		"value definition" in {
			"simple" in {
				val code = "val a = 34"
				computeResults(code) must beLike { case BlockResult(List(first)) =>
					first === ValDefResult("a", None, rhs = BlockResult(List(ExpressionResult(34, line = 1))), line = 1)
				}
			}
			"complex" in {
				val code = "val a = 2 * 3 - 2"
				computeResults(code) must beLike { case BlockResult(List(first)) =>
					first === ValDefResult("a", None, rhs = BlockResult(List(ExpressionResult(4, line = 1))), line = 1)
				}
			}
			"with use" in {
				val code = """val a = 34
							| a + 10""".stripMargin
				computeResults(code) must beLike { case BlockResult(List(first, second)) =>
					first === ValDefResult("a", None, rhs = BlockResult(List(ExpressionResult(34, line = 1))), line = 1)
					second === ExpressionResult(44, Nil, 2)
				}						
			}
			"value is function application" in {
				val code = """def perform(a: Int) = a + 5
							| val gg = perform(4)""".stripMargin
				computeResults(code) must beLike { case BlockResult(List(first, second)) =>
					first must beLike {case DefDefResult("perform", params, None, rhs, 1) =>
						rhs === BlockResult(List(ExpressionResult(8, Nil, 1)))
						params must beLike { case List(a) => structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
						}
					}
					second === ValDefResult("gg", None, rhs = BlockResult(List(ExpressionResult(9, line = 2))), line = 2)
				}
			}
		}
		
		"class definition" in {
			"simple" in {
				val code = "class Car(model: String, year: Int)"
				computeResults(code) must beLike { case BlockResult(List(classDef)) =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				}
			}
			"with instantiation" in {
				val code = """case class Car(model: String, year: Int)
							 | val test = Car("Fiat", 1999)""".stripMargin
				computeResults(code) must beLike { case BlockResult(List(classDef, valDef)) =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				 	valDef must beLike {case ValDefResult("test", None, rhs, 2) =>
						rhs must beLike { case BlockResult(List(ExpressionResult(ObjectResult(obj), Nil, 2))) =>
							obj.toString === "Car(Fiat,1999)"
				 		} 
					}					
				}
			}
			"with use" in {
				val code = """case class Car(model: String, year: Int)
							| val test = Car("Fiat", 1999)
							| test.model
							| test.year""".stripMargin
				computeResults(code) must beLike { case BlockResult(List(classDef, valDef, expression1, expression2)) =>
				 	classDef must beLike {case ClassDefResult("Car", params, BlockResult(Nil),1) =>
						params must beLike { case List(a, b) => 
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
				 		} 
					}
				 	valDef must beLike {case ValDefResult("test", None, rhs, 2) =>
						rhs must beLike { case BlockResult(List(ExpressionResult(ObjectResult(obj), Nil, 2))) =>
							obj.toString === "Car(Fiat,1999)"
				 		} 
					}
					expression1 === ExpressionResult("Fiat", Nil, 3)
					expression2 === ExpressionResult(1999, Nil, 4)					
				}
			}
			"complex" in {
				"1" in {
						val code = """case class Car(model: String, year: Int) {
									| 	def drive { println("vroum vroum")}
									| }
									| val a = new Car("BMW", 2013)
									| a.drive""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(classDef, valDef, expression1)) =>
						 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
						 		}
						 		body === BlockResult(List(DefDefResult("drive", Nil, None, BlockResult(List(ExpressionResult((), Nil, 2))), 2))) 
							}
						 	valDef must beLike {case ValDefResult("a", None, rhs, 4) =>
								rhs must beLike { case BlockResult(List(ExpressionResult(ObjectResult(obj), Nil, 4))) =>
									obj.toString === "Car(BMW,2013)"
						 		} 
							}
							expression1 === ExpressionResult((), Nil, 5)
						}
					}
				"2" in {
					val code = """case class Car(model: String, year: Int) {
								|	def license(seed: Int) = model.take(seed) + year + seed
								| }""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(classDef)) =>
						 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
						 		}
						 		body must beLike{case BlockResult(List(DefDefResult("license", params, None, BlockResult(List(ExpressionResult(ObjectResult("foo33"), Nil, 2))), 2))) =>
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
								| }""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(classDef)) =>
						 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
						 		}
						 		body === BlockResult(List(DefDefResult("license", Nil, None, BlockResult(List(ExpressionResult(5, Nil, 2))), 2))) 
							}
						}
					}
				"4" in {
					val code = """case class Car(model: String, year: Int) {
								|	val a = 5
								|	val b = (a + year).toString + model
								| }""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(classDef)) =>
						 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
						 		}
						 		body === BlockResult(List(
						 			ValDefResult("a", None, BlockResult(List(ExpressionResult(5, Nil, 2))), 2),
						 			ValDefResult("b", None, BlockResult(List(ExpressionResult("8foo", Nil, 3))), 3))) 
							}
						}
					}	
				"5" in {
					val code = """case class Car(model: String, year: Int) {
								|	val aabb = List(1,2,4,5).take(2)
								|	def wtv(a: Int) = aabb.drop(a)
								| }""".stripMargin
						computeResults(code) must beLike { case BlockResult(List(classDef)) =>
						 	classDef must beLike {case ClassDefResult("Car", params, body,1) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("model")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("year")), Literal(Constant(3))))
						 		}
						 		body must beLike{ case BlockResult(List( valDef, defDef)) =>
						 			valDef === ValDefResult("aabb", None, BlockResult(List(ExpressionResult(List(1, 2), Nil, 2))), 2)
						 			defDef must beLike {case DefDefResult("wtv", params, None, body, 3) => 
										params must beLike { case List(a) => 
											structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
								 		}
										body === BlockResult(List(ExpressionResult(Nil, Nil, 3)))
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
								| }""".stripMargin
					computeResults(code) ==== BlockResult(Nil)
				}
				"one function definition" in {
					val code = """abstract class Pet {
								|	def af(g: Int): Boolean
								| }""".stripMargin
					computeResults(code) ==== BlockResult(Nil)
				}
				"one function definition followed by evaluated expression" in {
					val code = """abstract class Pet {
								|	def af(g: Int): Boolean
								| }
								| val a = 5 + 5""".stripMargin
					computeResults(code) ==== BlockResult(List(ValDefResult("a", None, BlockResult(List(ExpressionResult(10, Nil, 4))), 4)))
				}
			}
		}
		"object definition" in {
			"with only simple values" in {
				val code = """object MySingleton {
							|	val a = 5
							|	val tretre = "goo"
							| }""".stripMargin
				computeResults(code) must beLike{ case BlockResult(List(moduleDef)) =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body === BlockResult(List(
							ValDefResult("a", None, BlockResult(List(ExpressionResult(5, Nil, 2))), 2),
							ValDefResult("tretre", None, BlockResult(List(ExpressionResult("goo", Nil, 3))), 3)
						))
					}
				}
			}
			"with only values" in {
				val code = """object MySingleton {
						|	val a = 5
						|	val b = a + 4
						| }""".stripMargin
				computeResults(code) must beLike{ case BlockResult(List(moduleDef)) =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body === BlockResult(List(
							ValDefResult("a", None, BlockResult(List(ExpressionResult(5, Nil, 2))), 2),
							ValDefResult("b", None, BlockResult(List(ExpressionResult(9, Nil, 3))), 3)
						))
					}
				}
			}
			"with function definition" in {
				val code = """object MySingleton {
							|	def jade(a: String, b: Int) = a + a
							| }""".stripMargin
				computeResults(code) must beLike{ case BlockResult(List(moduleDef)) =>
					moduleDef must beLike{ case ModuleDefResult("MySingleton", body, 1) =>
						body must beLike { case BlockResult(List(defDef)) =>
							defDef must beLike{ case DefDefResult("jade", params, None, rhs, 2) =>
								params must beLike { case List(a, b) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
									structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(3))))
								}
								rhs === BlockResult( List(ExpressionResult("foofoo", Nil, 2)))
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
				computeResults(code) must beLike{ case BlockResult(List(moduleDef)) =>
					moduleDef must beLike { case ModuleDefResult("MySingleton", body, 1) =>
						body must beLike { case BlockResult(List(valDef, defDef)) =>
							valDef === ValDefResult("aabb", None, BlockResult(List(ExpressionResult(57, Nil, 2))), 2)
							defDef must beLike{ case DefDefResult("blabla", params, None, rhs, 3) =>
								params must beLike { case List(a) => 
									structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant('a'))))
								}
								rhs === BlockResult( List(ExpressionResult("57a", Nil, 3)))
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
							| case class Cat(a: String) extends Animal
							| case class Dog(b: Int) extends Animal
							| def gogo(hui: Animal) = hui.toString""".stripMargin
					computeResults(code) must beLike { case BlockResult(List(cat, dog, gogo)) =>
						cat must beLike { case ClassDefResult("Cat", List(a), BlockResult(Nil), 2) =>
							structureEquals(a, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
						}
						dog must beLike { case ClassDefResult("Dog", List(b), BlockResult(Nil), 3) =>
							structureEquals(b, AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(3))))
						}
						gogo must beLike { case DefDefResult("gogo", List(hui), None, expr, 4) =>
							structureEquals(hui, AssignOrNamedArg(Ident(newTermName("hui")), Apply(Ident(newTermName("Cat")), List(Literal(Constant("foo"))))))
							expr ==== ExpressionResult("Cat(foo)", Nil, 4)
						}
					}
				}
				/*"no concrete class" in {
					"simple" in {
						val code = """abstract class Animal
									| def gogogo(lui: Animal) = 5 + 5""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("", "gogogo(lui = new Animal {}) => 10")
					}
					"with abstract members" in {
						"one value definition" in {
							val code = """abstract class Animal {
										|	val y: Int
										| }
										| def gogogi(yui: Animal) = yui.y + 45""".stripMargin
							ScalaCodeSheet.computeResults(code) ==== List("", "", "", "gogogi(yui = new Animal { val y = 3 }) => 48")
						}
						"two value definitions" in {
							val code = """abstract class Animal {
										|	val y: Int
										|	val z: String
										| }
										| def gogogi(yui: Animal) = yui.z + (yui.y + 45)""".stripMargin
							val expected = List("", "", "", "", """gogogi(yui = new Animal { val y = 3; val z = "foo" }) => foo48""")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
						"one function definition" in {
							val code = """abstract class Animal {
										|	def y(a: Int): Int
										| }
										| def gogogi(yui: Animal) = yui.y(3)""".stripMargin
							val expected = List("", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3 }) => 3")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
						"two function definitions" in {
							val code = """abstract class Animal {
										|	def y(a: Int): Int
										|	def z(b: Char): Boolean
										| }
										| def gogogi(yui: Animal) = if (yui.z('a')) yui.y(3) else 7""".stripMargin
							val expected = List("", "", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3; def z(b: Char) = true }) => 3")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
					}
				}
			}
			"trait" in {
				"simple hierarchy" in {
					val code = """trait Animal
								| case class Cat(a: Int) extends Animal
								| case class Dog(b: Boolean) extends Animal
								| def tti(bb: Animal) = bb.toString""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "", "tti(bb = Cat(3)) => Cat(3)")
				}
				"no concrete class" in {
					"simple" in {
						val code = """trait Animal
									| def tto(bb: Animal) = 5 + 4""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("", "tto(bb = new Animal {}) => 9")
					}
					"with abstract members" in {
						"one value definition" in {
							val code = """trait Animal {
										|	val y: Int
										| }
										| def gogogi(yui: Animal) = yui.y + 45""".stripMargin
							ScalaCodeSheet.computeResults(code) ==== List("", "", "", "gogogi(yui = new Animal { val y = 3 }) => 48")
						}
						"two value definitions" in {
							val code = """trait Animal {
										|	val y: Int
										|	val z: String
										| }
										| def gogogi(yui: Animal) = yui.z + (yui.y + 45)""".stripMargin
							val expected = List("", "", "", "", """gogogi(yui = new Animal { val y = 3; val z = "foo" }) => foo48""")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
						"one function definition" in {
							val code = """trait Animal {
										|	def y(a: Int): Int
										| }
										| def gogogi(yui: Animal) = yui.y(3)""".stripMargin
							val expected = List("", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3 }) => 3")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
						"two function definitions" in {
							val code = """trait Animal {
										|	def y(a: Int): Int
										|	def z(b: Char): Boolean
										| }
										| def gogogi(yui: Animal) = if (yui.z('a')) yui.y(3) else 7""".stripMargin
							val expected = List("", "", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3; def z(b: Char) = true }) => 3")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
					}
				}*/
			}
		}
		"???" in {
			"value definition" in {
				val code = "val a = ???"
				computeResults(code) must beLike { case BlockResult(List(ValDefResult("a", None, rhs, 1))) =>
					rhs ==== ExpressionResult(NotImplementedResult, Nil, 1)
				}
			}
			"function definition" in {
				"no params" in {
					val code = "def gog = ???"
					computeResults(code) must beLike { case BlockResult(List(DefDefResult("gog", Nil, None, rhs, 1))) =>
						rhs ==== ExpressionResult(NotImplementedResult, Nil, 1)
					}
				}
				"with params" in {
					val code = "def gogg(a: Int) = ???"
					computeResults(code) must beLike { case BlockResult(List(DefDefResult("gogg", params, None, rhs, 1))) =>
						params must beLike { case List(param) =>
							structureEquals(param, AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))))
						}
						rhs ==== ExpressionResult(NotImplementedResult, Nil, 1)
					}
				}
			}
		}
		"import" in {
			"simple" in {
				val code = """import scala.util.Random
							| if (Random.nextBoolean) 10 else 10""".stripMargin
				computeResults(code) ==== ExpressionResult(10, Nil, 2)
			}
			"relative" in {
				val code = """import scala._
							| import util.Random
							| if (Random.nextBoolean) 10 else 10""".stripMargin
				computeResults(code) ==== ExpressionResult(10, Nil, 3)
			}
		}
	}
}