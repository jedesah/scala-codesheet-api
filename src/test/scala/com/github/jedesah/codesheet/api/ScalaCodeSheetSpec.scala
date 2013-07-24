package com.github.jedesah.codesheet.api

import org.specs2.mutable._

class ScalaCodeSheetSpec extends Specification {
	"ScalaCodeSheet" should {
		"expressions" in {
			"literal" in {
				ScalaCodeSheet.computeResults("1") ==== List("1")
			}
			"typical" in {
				ScalaCodeSheet.computeResults("1 + 1") ==== List("2")
			}
			"two lines" in {
				val code = """1 + 1
							| 4 * 4""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("2", "16")
			}
			"with empty line" in {
				val code = """1 + 1
					|
					| 4 * 4""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("2", "", "16")
			}
			"expression returning Unit" in {
				val code = "if (true && false) 34"
				ScalaCodeSheet.computeResults(code) ==== List("")
			}
			"multiline expression" in {
				val code = """if (true || false)
							|	45 + 23""".stripMargin
				ScalaCodeSheet.computeResults(code) ==== List("68", "")
			}
		}

		"function definition" in {
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
									|	temp.dropRight(1)
									| }""".stripMargin
						val result = List(
							"""foo(a = List(3, 5, 7), b = 11) => List(11, 3, 5)""",
							"temp = List(3, 5, 7)",
							"almostDone = List(11, 3, 5, 7)",
							""
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
										| def gogogi(yui: Animal) = yui + 45""".stripMargin
							ScalaCodeSheet.computeResults(code) ==== List("", "", "", "gogogi(yui = new Animal { val y = 3 }) => 48")
						}
						"two values definitions" in {
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
							val expected = List("", "", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3 }) => 3")
							ScalaCodeSheet.computeResults(code) ==== expected
						}
						"two function definitions" in {
							val code = """abstract class Animal {
										|	def y(a: Int): Int
										|	def z(b: Char): Boolean
										| }
										| def gogogi(yui: Animal) = if (yui.z('a')) yui.y(3) else 7""".stripMargin
							val expected = List("", "", "", "", "gogogi(yui = new Animal { def y(a: Int) = 3; def z(b: Char) = trye }) => 3")
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
					ScalaCodeSheet.computeResults(code) ==== List("", "", "", "tti(bb = Cat(a = 3)) => Cat(3)")
				}
				"no concrete class" in {
					val code = """trait Animal
								| def tto(bb: Animal) = 5 + 4""".stripMargin
					ScalaCodeSheet.computeResults(code) ==== List("", "tto(bb = new Animal {}) => 9")
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
	}
}