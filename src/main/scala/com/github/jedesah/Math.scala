package com.github.jedesah

object Math {
	val primeNumbers = Stream.from(2).filter(isPrime)

    def isPrime(num: Int) = if (num < 2) false else !(2 until num).exists(num % _ == 0)
}