package com.github.jedesah

package object AugmentedCollections {

	implicit class AugmentedStringList(list: List[String]) {
		def safeUpdate(index: Int, value: String, padding: String = ""): List[String] =
			if (index < list.length) list.updated(index, value)
			else (list :+ padding).safeUpdate(index, value, padding)
	}
}