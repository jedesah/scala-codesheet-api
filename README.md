Scala CodeSheet API
===================

An API to evaluate Scala code. It will output a String representation of every top-level expression. Furthermore, the evaluation engine will generate sample values for functions and classes in order to output a reprensentation of expressions within these.

Right now the library accepts a String and outputs a String.

    import com.github.jedesah.codesheet.api.ScalaCodeSheet
    
    val myCode = "val a = ..."
    val result:String = ScalaCodeSheet.computeResults(myCode)
    
But there are plans to output a more structured representation of the result.

## Features

You can have a look at the extensive specification for what works and what doesn't here.

Here is a highlight of what it does right now

### Relatively simple stuff

#### input

    val a = 45 + 56
    val b = a + 2
    def foo(p: Int, z: Boolean) = if (z) p else p -1
    foo(b, false)
    
#### output

    a = 101
    b = 103
    foo(p = 3, z = true) => 3
    102
    
### What about my own types

#### input

    case class Person(name: String, age: Int)
    def isElligible(person: Person, localLegalAdultAge: Int = 18) = {
        val isAdult = person.age > localLegalAdultAge
        isAdult || person.startsWith("Sa")
    }
    isElligible(Person("Sam", 2))
    
#### output

    
    isElligible(person = Person("foo", 3), localLegalAdultAge = 18) => false
    isAdult = false
    false
    true
    
### What about inheritance?

#### input

    abstract class Animal {
        def communicate: String
    }
    class Dog extends Animal {
        def communicate = "Wouf!"
    }
    class Cat extends Animal {
        def communicate = "Miow"
    }
    provoke(animal: Animal) = animal.communicate
    
#### output

    
    
    
    
    
    
    
    
    
    provoke(animal = new Dog) => Wouf!
    
### Arguably useless stuff

#### input

    abstract class Animal {
        def communicate: String
    }
    provoke(animal: Animal) = animal.communicate
    
#### output

    
    
    provoke(animal = new Animal { def communicate = "foo" }) = foo
    
