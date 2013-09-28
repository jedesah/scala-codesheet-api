package com.github.jedesah

object ScalaUtils {
	def toSource(s: String) = "\"" + s + "\""

	def toSource(prim: AnyVal) = prim.toString

	def toSource(a: Any): Option[String] =
		a match {
			case p: Product => {
				val childs = p.productIterator.map(toSource)
				if (childs.contains(None)) None
				else Some(childs.flatten mkString (p.productPrefix + "(", ", ", ")"))
			}
			case s: String => Some(toSource(s))
			case other =>
				if (testAnyVal(other)) Some(toSource(other.asInstanceOf[AnyVal]))
				else None
		} 

	def testAnyVal[T](x: T)(implicit evidence: T <:< AnyVal = null) = evidence != null

}