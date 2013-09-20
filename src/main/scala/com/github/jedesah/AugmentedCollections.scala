package com.github.jedesah

package object AugmentedCollections {

	implicit class AugmentedString(string: String) {
		def tabulate: String = {
			val impl = (line: String) => if (line == "") "" else "\t" + line
			string.lines.map(impl).mkString("\n")
		}
	}

	implicit class AugmentedStream[T](stream: Stream[T]) {
		def repeat: Stream[T] = stream append stream.repeat
	}
}