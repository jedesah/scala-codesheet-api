Scala CodeSheet API
===================

An API to evaluate Scala code. It will output a String representation of every top-level expression. Furthermore, the evaluation engine will generate sample values for functions and classes in order to output a representation of expressions within these.

Right now the library accepts a String and outputs a String.

    import com.github.jedesah.codesheet.api.ScalaCodeSheet
    
    val myCode = "val a = ..."
    val result: List[String] = ScalaCodeSheet.computeResults(myCode)
    
But there are plans to output a more structured representation of the result.

# Features

You can have a look at the extensive specification for what works and what doesn't [here](https://github.com/jedesah/scala-codesheet-api/blob/master/src/test/scala/com/github/jedesah/codesheet/api/ScalaCodeSheetSpec.scala).

If there isn't a test for it, you can probably assume it doesn't work.

Here are some highlights of what it can do right now.

### Relatively simple stuff

    val a = 45 + 56                                       | a = 101
    val b = a + 2                                         | b = 103
    def foo(p: Int, z: Boolean) = if (z) p else p -1      | foo(p = 3, z = true) => 3 
    foo(b, false)                                         | 102
    
### What about my own types

    case class Person(name: String, age: Int)             |
    def isElligible(guy: Person, legalAge: Int = 18) = {  | isElligible(guy = Person("foo", 3), legalAge = 18) => false
        val isAdult = guy.age > legalAge                  | isAdult = false
        isAdult || guy.startsWith("Sa")                   | false
    }                                                     |
    isElligible(Person("Sam", 2))                         | true
    
### What about inheritance?

    abstract class Animal {                               |
        def communicate: String                           |
    }                                                     |
    class Dog extends Animal {                            |
        def communicate = "Wouf!"                         |
    }                                                     |
    class Cat extends Animal {                            |
        def communicate = "Miow"                          |
    }                                                     |
    provoke(animal: Animal) = animal.communicate          | provoke(animal = new Dog) => Wouf!
    
### Arguably useless stuff

    abstract class Animal {                               |
        def communicate: String                           |
    }                                                     |
    provoke(animal: Animal) = animal.communicate          | provoke(animal = new Animal { def communicate = "foo" }) = foo
