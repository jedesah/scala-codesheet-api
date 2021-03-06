package com.github.jedesah

import AugmentedCollections.AugmentedSeq

object ScalaUtils {
	def toSource(s: String) = "\"" + s + "\""
	def toSource(c: Char) = "'" + c + "'"
	def toSource(prim: AnyVal) = prim.toString
	def toSource(a: Any): Option[String] =
		a match {
			case p: Product => {
				val children = p.productIterator.toList.map(toSource)
				if (children.contains(None)) None
				else Some(p.productPrefix + children.flatten.mkStringNoEndsIfEmpty("(", ", ", ")"))
			}
			case s: String => Some(toSource(s))
			case c: Char => Some(toSource(c))
			case 
				  _ : Unit 
				| _ : Boolean 
				| _ : Byte
				| _ : Short 
				| _ : Int
				| _ : Long
				| _ : Float
				| _ : Double
				| _ : AnyRef => Some(a.toString)
			case other => None
		} 
}