package com.github.jedesah.codesheet

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.Flag._

package object api {

	def isConstructor(tree: Tree) = tree match {
		case defdef: DefDef => defdef.name == nme.CONSTRUCTOR
		case _ => false
	}

	implicit class AugmentedTree(tree: Tree) {

		val notImplSymbol = Ident(newTermName("$qmark$qmark$qmark"))

    	def prettyPrint: String = {
	        // I believe this is not necessary anymore
	        /*case tree: Apply => tree.fun match {
	            case fun: Select => fun.qualifier.toString + "(" + tree.args.mkString(", ") + ")"
	            case fun: Ident => fun.toString + "(" + tree.args.mkString(", ") + ")"
	        }*/
	        deconstructAnonymous(tree).map { case(name, body) =>
	            val bodyString = if (body.isEmpty) "{}" else s"""{ ${body.mkString("; ")} }"""
	            s"new $name $bodyString"
	        }.getOrElse {
	            tree match {
	                case Apply(Select(x, newTermName), List(y)) => s"$x ${newTermName.decoded} $y"
	                case _ => tree.toString
	            }
	        }
	    }
	}

  implicit class AugmentedMemberDef(memberDef: MemberDef) {
    val isAbstract = memberDef.mods.hasFlag(ABSTRACT)
  }

	implicit class AugmentedClassDef(classDef: ClassDef) {
		val constructorOption = classDef.impl.body.find(isConstructor).asInstanceOf[Option[DefDef]]
		val isCaseClass = classDef.mods.hasFlag(CASE)
		val isTrait = classDef.mods.hasFlag(INTERFACE) && classDef.mods.hasFlag(ABSTRACT)
		val abstractMembers = classDef.impl.body.collect { case def_ : ValOrDefDef if def_.rhs.isEmpty => def_ }
	}

	implicit class AugmentedModuleDef(moduleDef: ModuleDef) {
		val constructor = moduleDef.impl.body.find(isConstructor).get.asInstanceOf[DefDef]
	}

	def anonClass(name: String, impl: List[ValOrDefDef] = Nil) = Block(List(ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(Ident(newTypeName(name))), emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(()))))) ::: impl))), Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), Nil))
    def deconstructAnonymous(tree: Tree): Option[(String, List[ValOrDefDef])] = tree match {
        case block: Block => block.children(0) match {
            case classDef: ClassDef => {
                val parentName = classDef.impl.parents(0).toString
                val body = classDef.impl.body.collect { case valOrDef: ValOrDefDef if !isConstructor(valOrDef) => valOrDef }
                Some((parentName, body))
            }
            case _ => None
        }
        case _ => None
    }
}