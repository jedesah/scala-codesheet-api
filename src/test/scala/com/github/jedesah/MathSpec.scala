package com.github.jedesah

import org.specs2.mutable._

class MathSpec extends Specification {
	"Math" should {
		"isPrime" in {
			Math.isPrime(1) ==== false
			Math.isPrime(2) ==== true
			Math.isPrime(3) ==== true
			Math.isPrime(4) ==== false
			Math.isPrime(5) ==== true
			Math.isPrime(100) ==== false
			Math.isPrime(101) ==== true
		}

		"primeNumbers" in {
			Math.primeNumbers.take(10).toList ==== List(2,3,5,7,11,13,17,19,23,29)
		}
	}
}