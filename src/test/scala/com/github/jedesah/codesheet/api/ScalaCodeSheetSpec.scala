package com.github.jedesah.codesheet.api

import org.specs2.mutable._

class ScalaCodeSheetSpec extends Specification {
	"ScalaCodeSheet" should {
		"expressions" in {
			"literal" in {
				ScalaCodeSheet.computeResults("1") ==== List("1")
			}
			"complex" in {
				ScalaCodeSheet.computeResults("1 + 1") ==== List("2")
			}
			"two" in {
				"no newline" in {
					val code = """1 + 1
						| 4 * 4""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("2", "16")
				}
				"with newline" in {
					val code = """1 + 1
						|
						| 4 * 4""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("2", "", "16")
				}
			}
		}

		"function" in {
			"simple definition" in {
				val code = """def hello = "Hello!" """
				ScalaCodeSheet.computeResults(code) ==== List("")
			}
			"complex definition" in {
				val code = "def foo = 10 * 4 - 2"
				ScalaCodeSheet.computeResults(code) ==== List("foo => 38")
			}
			"with use" in {
				val code = """def hello = "Hello!"
							|
							| hello""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("", "", "Hello!")
			}
			"with params" in {
				"basic types" in {
					"Int" in {
						val code = "def foo(a: Int, b: Int, c: Int) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 3, b = 5, c = 7) => 1")
					}
					"String" in {
						val code = """def addExclamation(a: String, b: String, c: String) = s"$a! $b! $c!" """
						ScalaCodeSheet.computeResults(code) ==== List("""addExclamation(a = "foo", b = "bar", c = "biz") => foo! bar! biz!""")
					}
					"Float" in {
						val code = "def foo(a: Float, b: Float, c: Float) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 1.5, b = 2.5, c = 4.5) => -0.5")
					}
					"Boolean" in {
						val code = "def foo(a: Boolean, b: Boolean, c: Boolean) = a && b || c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = true, b = false, c = true) => true")
					}
					"Long" in {
						val code = "def foo(a: Long, b: Long, c: Long) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 3, b = 5, c = 7) => 1")
					}
					"Double" in {
						val code = "def foo(a: Double, b: Double, c: Double) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 1.5, b = 2.5, c = 4.5) => -0.5")
					}
					"Byte" in {
						val code = "def foo(a: Byte, b: Byte, c: Byte) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 3, b = 5, c = 7) => 1")
					}
					"Short" in {
						val code = "def foo(a: Short, b: Short, c: Short) = a + b - c"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = 3, b = 5, c = 7) => 1")
					}
					"Char" in {
						val code = """def addExclamation(a: Char, b: Char, c: Char) = s"$a! $b! $c!" """
						ScalaCodeSheet.computeResults(code) ==== List("addExclamation(a = 'a', b = 'b', c = 'c') => a! b! c!")
					}
					"AnyVal" in {
						val code = """def foo(a: AnyVal, b: AnyVal, c: AnyVal) = s"$a! $b! $c!" """
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = 'f', c = true) => 3! f! true!""")
					}
					"Any" in {
						val code = """def foo(a: Any, b: Any, c: Any) = s"$a! $b! $c!" """
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3! foo! true!""")
					}
					"AnyRef" in {
						val code = """def bar(a: AnyRef, b: AnyRef, c: AnyRef) = s"$a! $b! $c!" """
						ScalaCodeSheet.computeResults(code) ==== List("""bar(a = "foo", b = List(3, 5, 7), c = Some(5)) => foo! List(3, 5, 7)! Some(5)!""")
					}
					"List" in {
						val code = "def foo(a: List[Int], b: List[Int], c: List[Int]) = if (c.isEmpty) a.length else b.length"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = List(3, 5, 7), b = Nil, c = List(11)) => 0")
					}
					"Option" in {
						val code = "def foo(a: Option[String], b: Option[String], c: Option[String]) = if(c.isEmpty) b else a"
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = Some("foo"), b = None, c = Some("bar")) => Some(foo)""")
					}
					"Seq" in {
						val code = "def foo(a: Seq[Boolean], b: Seq[Boolean], c: Seq[Boolean]) = if (c.isEmpty) a.length else b.take(1)"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = List(true, false, true), b = Nil, c = List(false)) => List()")
					}
					"wildCardGeneric" in {
						val code = "def foo(a: List[_], b: List[_], c: List[_]) = if (c.nonEmpty) a else b"
						ScalaCodeSheet.computeResults(code) ==== List("foo(a = List(3,5,7), b = Nil, c = List(true)) => List(3,4,7)")
					}
				}
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
							pending
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
					"1 line" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
									| 	if (c) a else b.length""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3""", "3")
					}
					"2 line" in {
						val code = """def foo(a: Int, b: String, c: Boolean) =
									| 	if (c)
									|		a
									|	else
									|		b""".stripMargin
						ScalaCodeSheet.computeResults(code) ==== List("""foo(a = 3, b = "foo", c = true) => 3""", "true", "3", "", "foo")
					}
					"more complex" in {
						val code = """def foo(a: List[Int], b: String, c: String, d: Boolean, e: Boolean) =
									| 	if (d && e || a.length == 2)
									|		b.take(2) + c.drop(3)
									|	else
									|		b.take(3) + c.drop(1)""".stripMargin
						val result = List(
							"""foo(a = List(1,2,3), b = "foo", c = "bar", d = true, e = false) => foobar""",
							"false",
							"fba",
							"",
							"foobar"
						)
						ScalaCodeSheet.computeResults(code) ==== result
					}
				}
			}	
		}
		
		"value definition" in {
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
			}
		}
	}
}