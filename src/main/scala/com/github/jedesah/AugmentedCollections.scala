package com.github.jedesah

package object AugmentedCollections {

	}

	implicit class AugmentedStream[T](stream: Stream[T]) {
		def repeat: Stream[T] = stream append stream.repeat
	}
}