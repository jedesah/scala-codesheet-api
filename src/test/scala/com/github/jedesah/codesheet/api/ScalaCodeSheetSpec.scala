package com.github.jedesah.codesheet.api

import org.specs2.mutable._
import ScalaCodeSheet._
import scala.reflect.runtime.universe._

class ScalaCodeSheetSpec extends Specification {
	"ScalaCodeSheet" should {
		"expressions" in {
			"literal" in {
				computeResults("1") ==== BlockResult(List(ExpressionResult(1, line = 1)))
			}
			"typical" in {
				computeResults("1 + 1") ==== BlockResult(List(ExpressionResult(2, line = 1)))
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
				val first = ValDefResult("a", None, inner = BlockResult(List(ExpressionResult(4, line = 1))), line = 1)
				val params = List(AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))))
				val inner = BlockResult(List(ExpressionResult("foofoo", line = 2)))
				val second = DefDefResult("tot", inferredType = None, params = params, inner = inner, line = 2)
				computeResults(code) ==== BlockResult(List(first, second))
			}
			"with params" in {
				"basic types" in {
					"Int" in {
						val code = "def foo(a: Int, b: Int, c: Int) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7)))
						)
						val inner = BlockResult(List(ExpressionResult(1, line = 1))) 
						computeResults(code) ====  BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))
					}
					"String" in {
						val code = """def addExclamation(a: String, b: String, c: String) = s"$a! $b! $c!" """
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("bar"))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant("biz")))
						)
						val inner = BlockResult(List(ExpressionResult("foo! bar! biz!", line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("addExclamation", params, None, inner, line = 1)))
					}
					"Float" in {
						val code = "def foo(a: Float, b: Float, c: Float) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(1.5))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(2.5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(4.5)))
						)
						val inner = BlockResult(List(ExpressionResult(-0.5, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Boolean" in {
						val code = "def foo(a: Boolean, b: Boolean, c: Boolean) = a && b || c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(true))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(false))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true)))
						)
						val inner = BlockResult(List(ExpressionResult(true, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Long" in {
						val code = "def foo(a: Long, b: Long, c: Long) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7)))
						)
						val inner = BlockResult(List(ExpressionResult(1, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Double" in {
						val code = "def foo(a: Double, b: Double, c: Double) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(1.5))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(2.5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(4.5)))
						)
						val inner = BlockResult(List(ExpressionResult(-0.5, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Byte" in {
						val code = "def foo(a: Byte, b: Byte, c: Byte) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7)))
						)
						val inner = BlockResult(List(ExpressionResult(1, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Short" in {
						val code = "def foo(a: Short, b: Short, c: Short) = a + b - c"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant(5))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(7)))
						)
						val inner = BlockResult(List(ExpressionResult(1, line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Char" in {
						val code = """def addExclamation(a: Char, b: Char, c: Char) = s"$a! $b! $c!" """
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant('a'))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant('b'))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant('c')))
						)
						val inner = BlockResult(List(ExpressionResult("a! b! c!", line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("addExclamation", params, None, inner, line = 1)))
					}
					"AnyVal" in {
						val code = """def foo(a: AnyVal, b: AnyVal, c: AnyVal) = s"$a! $b! $c!" """
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant('f'))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true)))
						)
						val inner = BlockResult(List(ExpressionResult("3! f! true!", line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Any" in {
						val code = """def foo(a: Any, b: Any, c: Any) = s"$a! $b! $c!" """
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant(3))),
							AssignOrNamedArg(Ident(newTermName("b")), Literal(Constant("foo"))),
							AssignOrNamedArg(Ident(newTermName("c")), Literal(Constant(true)))
						)
						val inner = BlockResult(List(ExpressionResult("3! foo! true!", line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"AnyRef" in {
						val code = """def bar(a: AnyRef, b: AnyRef, c: AnyRef) = s"$a! $b! $c!" """
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Literal(Constant("foo"))),
							AssignOrNamedArg(Ident(newTermName("b")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))),
							AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Some")), List(Literal(Constant(5)))))
						)
						val inner = BlockResult(List(ExpressionResult("foo! List(3, 5, 7)! Some(5)!", line = 1)))					
						computeResults(code) ==== BlockResult(List(DefDefResult("bar", params, None, inner, line = 1)))	
					}
					"List" in {
						val code = "def foo(a: List[Int], b: List[Int], c: List[Int]) = if (c.isEmpty) a.length else b.length"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))),
							AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))),
							AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("List")), List(Literal(Constant(11)))))
						)
						val inner = BlockResult(List(ExpressionResult(0, line = 1)))				
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Option" in {
						val code = "def foo(a: Option[String], b: Option[String], c: Option[String]) = if(c.isEmpty) b else a"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Some")), List(Literal(Constant("foo"))))),
							AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("None"))),
							AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Some")), List(Literal(Constant("bar")))))
						)
						val inner = BlockResult(List(ExpressionResult(Some("foo"), line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"Seq" in {
						val code = "def foo(a: Seq[Boolean], b: Seq[Boolean], c: Seq[Boolean]) = if (c.isEmpty) a.length else b.take(1)"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(true)), Literal(Constant(false)), Literal(Constant(true))))),
							AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))),
							AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("Seq")), List(Literal(Constant(false)))))
						)
						val inner = BlockResult(List(ExpressionResult(List(), line = 1)))						
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
					"wildCardGeneric" in {
						val code = "def foo(a: List[_], b: List[_], c: List[_]) = if (c.nonEmpty) a else b"
						val params = List(
							AssignOrNamedArg(Ident(newTermName("a")), Apply(Ident(newTermName("List")), List(Literal(Constant(3)), Literal(Constant(5)), Literal(Constant(7))))),
							AssignOrNamedArg(Ident(newTermName("b")), Ident(newTermName("Nil"))),
							AssignOrNamedArg(Ident(newTermName("c")), Apply(Ident(newTermName("List")), List(Literal(Constant(true)))))
						)
						val inner = BlockResult(List(ExpressionResult(List(3,5,7), line = 1)))		
						computeResults(code) ==== BlockResult(List(DefDefResult("foo", params, None, inner, line = 1)))	
					}
				}/*
				"custom class" in {
					"case" in {
						"one case class definition" in {
							"one param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car) = a.model + "-" + a.year""".stripMargin
								ScalaCodeSheet.computeResults(code) ==== List("", """foo(a = Car(3, "foo")) => foo-3""")
							}
							"two param occurence" in {
								val code = """case class Car(year: Int, model: String)
										| def foo(a: Car, b: Car) = a.year - b.year""".stripMargin
								ScalaCodeSheet.computeResults(code) ==== List("", """foo(a = Car(3, "foo"), b = Car(5, "bar")) => -2""")
							}
						}
						"two case class definitions" in {
							"use of first one" in {
								val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | def isMatch(car: Car) = car.year""".stripMargin
								val result = List(
									"",
									"",
									"""isMatch(car = Car(3, "foo")) => 3"""
								)
								ScalaCodeSheet.computeResults(code) ==== result
							}
							"use of second one" in {
								val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | def isMatch(person: Person) = person.name""".stripMargin
								val result = List(
									"",
									"",
									"""isMatch(person = Person("foo", 3)) => foo"""
								)
								ScalaCodeSheet.computeResults(code) ==== result
							}
						}
						"multiple case class definitions" in {
							"one param occurence" in {
								"case 1" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(doc: Document) = doc.author""".stripMargin
									val result = List(
										"",
										"",
										"",
										"""isMatch(doc = Document("foo", "bar")) => bar"""
									)
									ScalaCodeSheet.computeResults(code) ==== result
								}
								"case 2" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(person: Person) = person.name""".stripMargin
									val result = List(
										"",
										"",
										"",
										"""isMatch(person = Person("foo", 3)) => foo"""
									)
									ScalaCodeSheet.computeResults(code) ==== result
								}
								"case 3" in {
									val code = """case class Car(year: Int, model: String)
										  | case class Person(name: String, age: Int)
										  | case class Document(text: String, author: String)
										  | def isMatch(car: Car) = car.year""".stripMargin
									val result = List(
										"",
										"",
										"",
										"""isMatch(car = Car(3, "foo")) => 3"""
									)
									ScalaCodeSheet.computeResults(code) ==== result
								}
							}
							"multiple occurences" in {
								val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | case class Document(text: String, author: String)
									  | def isMatch(doc: Document, peps: Person) = doc.author == peps.name""".stripMargin
								val result = List(
									"",
									"",
									"",
									"""isMatch(doc = Document("foo", "bar"), peps = Person("biz", 3)) => false"""
								)
								ScalaCodeSheet.computeResults(code) ==== result
							}
						}
						"multiple function definitions" in {
							val code = """case class Car(year: Int, model: String)
									  | case class Person(name: String, age: Int)
									  | case class Document(text: String, author: String)
									  | def isMatch(doc: Document, peps: Person) = doc.author == peps.name
									  | def barCode(car: Car) = car.model + "-" + car.year""".stripMargin
							val result = List(
								"",
								"",
								"",
								"""isMatch(doc = Document("foo", "bar"), peps = Person("biz", 3)) => false""",
								"""barCode(car = Car(3, "foo")) => foo-3"""
							)
							ScalaCodeSheet.computeResults(code) ==== result
						}
					}
					"normal" in {
						"one" in {
							val code = """class Car(val year: Int, val model: String)
										| def foo(a: Car) = a.model + "-" + a.year""".stripMargin
							ScalaCodeSheet.computeResults(code) ==== List("", """foo(a = new Car(3, "foo")) => foo-3""")
						}
						"three" in {
							val code = """class Car(val year: Int, val model: String)
										| class Hut(oui: Boolean, tot: String)
										| class Fat(var calorie: Int, var heat: Int)
										| def foo(a: Car, b: Hut, c: Fat) = a.model + "-" + a.year""".stripMargin
							var result = List(
								"",
								"",
								"",
								"""foo(a = new Car(3, "foo"), b = new Hut(true, "bar"), c = new Fat(5, 7)) => foo-3"""
							)
							ScalaCodeSheet.computeResults(code) ==== result
						}
					}
				}
				"mixed" in {
					val code = """def foo(a: Int, b: String, c: Boolean) = if (c) a else b.length"""
					ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3""")
				}
				"with default Values" in {
					val code = "def foo(a: Int, b: Int = 10, c: Int = 1) = a + b - c"
					ScalaCodeSheet.computeResults(code) ==== List("foo(a = 3, b = 10, c = 1) => 12")
				}
				"multiline" in {
					"one expression" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
									| 	if (c) a else b.length""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3""", "")
					}
					"one expression on 4 lines" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
									| 	if (c)
									|		a
									|	else
									|		b""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3""", "", "", "", "")
					}
					"one complex expression on 4 lines" in {
						val code = """def foo(a: List[Int], b: String, c: String, d: Boolean, e: Boolean) =
									| 	if (d && e || a.length == 2)
									|		b.take(2) + c.drop(3)
									|	else
									|		b.take(3) + c.drop(1)""".stripMargin
						val result = List(
							"""foo(a = List(3, 5, 7), b = "foo", c = "bar", d = true, e = false) => fooar""",
							"",
							"",
							"",
							""
						)
						ScalaCodeSheet.computeResults(code) ==== result
					}
					"multiple inner value definition" in {
						val code = """def foo(a: List[Int], b: Int) = {
									|	val temp = a.take(4)
									|	val almostDone = b :: temp
									|	almostDone.dropRight(1)
									| }""".stripMargin
						val result = List(
							"""foo(a = List(3, 5, 7), b = 11) => List(11, 3, 5)""",
							"temp = List(3, 5, 7)",
							"almostDone = List(11, 3, 5, 7)",
							"List(11, 3, 5)",
							""
						)
						ScalaCodeSheet.computeResults(code) ==== result
					}
				}*/
			}	
		}
		
		/*"value definition" in {
			"simple" in {
				val code = "val a = 34"
				ScalaCodeSheet.computeResults(code) ==== List("")
			}
			"complex" in {
				val code = "val a = 2 * 3 - 2"
				ScalaCodeSheet.computeResults(code) ==== List("a = 4")
			}
			"with use" in {
				val code = """val a = 34
							| a + 10""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "44")
			}
			"value is function application" in {
				val code = """def perform(a: Int) = a + 5
							| val gg = perform(4)""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("perform(a = 3) => 8", "gg = 9")
			}
		}

		"class definition" in {
			"simple" in {
				val code = "class Car(model: String, year: Int)"
				ScalaCodeSheet.computeResults(code) ==== List("")
			}
			"with instantiation" in {
				val code = """case class Car(model: String, year: Int)
							| val test = Car("Fiat", 1999)""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "")
			}
			"with use" in {
				val code = """case class Car(model: String, year: Int)
							| val test = Car("Fiat", 1999)
							| test.model
							| test.year""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "", "Fiat", "1999")
			}
			"complex" in {
				"1" in {
					val code = """case class Car(model: String, year: Int) {
								| 	def drive { println("vroum vroum")}
								| }
								| val a = new Car("BMW", 2013)
								| a.drive""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "", "", "")
				}
				"2" in {
					val code = """case class Car(model: String, year: Int) {
								|	def license(seed: Int) = model.take(seed) + year + seed
								| }""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("""Car(model = "foo", year = 3) {""", "license(seed = 3) => foo33", "}")
				}
				"3" in {
					val code = """case class Car(model: String, year: Int) {
								|	def license = 5
								| }""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "")
				}
				"4" in {
					val code = """case class Car(model: String, year: Int) {
								|	val a = 5
								|	val b = (a + year).toString + model
								| }""".stripMargin
					val expected = List("""Car(model = "foo", year = 3) {""", "", "b = 8foo", "}")
					ScalaCodeSheet.computeResults(code) ==== expected
				}
				"5" in {
					val code = """case class Car(model: String, year: Int) {
								|	val aabb = List(1,2,4,5).take(2)
								|	def wtv(a: Int) = aabb.drop(a)
								| }""".stripMargin
					val expected = List("""Car(model = "foo", year = 3) {""", "aabb = List(1, 2)", "wtv(a = 3) => List()", "}")
					ScalaCodeSheet.computeResults(code) ==== expected
				}
			}
			"abstract" in {
				"one value definition" in {
					val code = """abstract class Pug {
								|	val a: Int
								| }""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "")
				}
				"one function definition" in {
					val code = """abstract class Pet {
								|	def af(g: Int): Boolean
								| }""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "")
				}
				"one function definition followed by evaluated expression" in {
					val code = """abstract class Pet {
								|	def af(g: Int): Boolean
								| }
								| val a = 5 + 5""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "", "a = 10")
				}
			}
		}

		"object definition" in {
			"with only simple values" in {
				val code = """object MySingleton {
							|	val a = 5
							|	val tretre = "goo"
							| }""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "", "", "")
			}
			"with only values" in {
				val code = """object MySingleton {
						|	val a = 5
						|	val b = a + 4
						| }""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("MySingleton {", "", "b = 9", "}")
			}
			"with function definition" in {
				val code = """object MySingleton {
							|	def jade(a: String, b: Int) = a + a
							| }""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("MySingleton {", """jade(a = "foo", b = 3) => foofoo""", "}")
			}
			"with values and definitions" in {
				val code = """object MySingleton {
							|	val aabb = 45 + 12
							|	def blabla(a: Char) = aabb.toString + a
							| }""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("MySingleton {", "aabb = 57", "blabla(a = 'a') => 57a", "}")
			}
		}
		"sample generation" in {
			"abstract class" in {
				"simple hierarchy" in {
					val code = """abstract class Animal
							| case class Cat(a: String) extends Animal
							| case class Dog(b: Int) extends Animal
							| def gogo(hui: Animal) = hui.toString""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "", "", """gogo(hui = Cat("foo")) => Cat(foo)""")
				}
				"no concrete class" in {
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
				}
			}
		}
		"???" in {
			"value definition" in {
				val code = "val a = ???"
				ScalaCodeSheet.computeResults(code) ==== List("")
			}
			"function definition" in {
				"no params" in {
					val code = "def gog = ???"
					ScalaCodeSheet.computeResults(code) ==== List("")
				}
				"with params" in {
					val code = "def gogg(a: Int) = ???"
					ScalaCodeSheet.computeResults(code) ==== List("gogg(a = 3) => ???")
				}
			}
		}
		"import" in {
			"simple" in {
				val code = """import scala.util.Random
							| if (Random.nextBoolean) 10 else 10""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "10")
			}
			"relative" in {
				val code = """import scala._
							| import util.Random
							| if (Random.nextBoolean) 10 else 10""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "import scala.util.Random", "10")
			}
		}*/
	}
}