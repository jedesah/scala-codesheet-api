package com.github.jedesah

object ScalaUtils {
	def toSource(s: String) = "\"" + s + "\""

	def toSource(prim: AnyVal) = prim.toString

	def toSource(a: Any): Option[String] =
		a match {
			case p: Product => {
				val childs = p.productIterator.toList.map(toSource)
				if (childs.contains(None)) None
				else Some(childs.flatten mkString (p.productPrefix + "(", ", ", ")"))
			}
			case s: String => Some(toSource(s))
			case 
				  _ : Unit 
				| _ : Boolean 
				| _ : Byte
				| _ : Char 
				| _ : Short 
				| _ : Int
				| _ : Long
				| _ : Float
				| _ : Double
				| _ : AnyRef => Some(a.toString)
			case other => None
		} 
}