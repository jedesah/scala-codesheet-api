package com.github.jedesah.codesheet

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.Flag._

package object api {

	def isConstructor(tree: Tree) = tree match {
		case defdef: DefDef => defdef.name == nme.CONSTRUCTOR
		case _ => false
	}

	implicit class AugmentedClassDef(classDef: ClassDef) {
		val constructorOption = classDef.impl.body.find(isConstructor).asInstanceOf[Option[DefDef]]
		val isCaseClass = classDef.mods.hasFlag(CASE)
		val isAbstract = classDef.mods.hasFlag(ABSTRACT)
		val abstractMembers = classDef.impl.body.collect { case def_ : ValOrDefDef if def_.rhs.isEmpty => def_ }
	}

	implicit class AugmentedModuleDef(moduleDef: ModuleDef) {
		val constructor = moduleDef.impl.body.find(isConstructor).get.asInstanceOf[DefDef]
	}
}