package com.github.jedesah.codesheet.api

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe.Flag._

import com.github.jedesah.Math
import com.github.jedesah.ScalaUtils._
import com.github.jedesah.AugmentedCollections._

import scala.reflect.internal.util._
import scala.tools.nsc.interactive.Response

object ScalaCodeSheet {

	val reporter = new scala.tools.nsc.reporters.StoreReporter()
	val settings = new scala.tools.nsc.Settings()
	settings.classpath.value = System.getProperty("replhtml.class.path")
	val compiler: scala.tools.nsc.interactive.Global = new scala.tools.nsc.interactive.Global(settings, reporter)

	val importer0 = ru.mkImporter(compiler)
	val importer = importer0.asInstanceOf[ru.Importer { val from: compiler.type; def importModifiers(mods: compiler.Modifiers): Modifiers; def importName(name: compiler.Name): Name }]

	val reservedName = "youcannotnameyourvariablelikethisincodesheet"
	val nameOfMap = reservedName + "_map"
	val nameOfTemp = reservedName + "_temp"

	case class Result(treesWithValues: List[compiler.Tree], output: String) {
		def userRepr = ???
	}

	case class SimpleExpressionResult(final_ : ValueResult, steps: List[Tree] = Nil) {
		def userRepr = (steps.map(_.prettyPrint) :+ final_.userRepr).mkString(" => ")
		def value = final_.valueOption
		def wasEvaluated = !final_.isInstanceOf[PlaceHolder]
	}
	trait ValueResult {
		def userRepr: String
		def valueOption: Option[Any]
	}
	case class ExceptionValue(ex: Throwable) extends ValueResult {
		def userRepr = "throws " + ex
		def valueOption = Some(ex)
	}
	case object NotImplementedResult extends ValueResult {
		def userRepr = "???"
		def valueOption = Some(new NotImplementedError())
	}
	case class ObjectResult(value: Any) extends ValueResult {
		def userRepr = value match {
			case _: scala.runtime.BoxedUnit | scala.Unit => ""
			case _ => value.toString
		}
		def valueOption = Some(value)
	}
	private case class PlaceHolder(id: Int) extends ValueResult {
		def userRepr = "" // TODO: Probably want to put this back to throw an error
		def valueOption = None
	}
	implicit def createObjectResult(value: Any) = ObjectResult(value)

	val notImplSymbol = Ident(newTermName("$qmark$qmark$qmark"))

	def analyseAndTransform(AST: List[compiler.Tree], enableSteps: Boolean): Tree = {

		var index = 0

		def wrapInTry(toWrap: Tree): Tree = Apply(Select(Select(Ident(newTermName("scala")), newTermName("util")), newTermName("Try")), List(toWrap))

		def evaluateValDef(AST: compiler.ValDef, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): List[Tree] = {
			if (AST.mods.hasFlag(compiler.Flag.SYNTHETIC)) List(importer.importTree(AST))
			else {
				val topLevelGoingForward = topLevel && !AST.mods.hasFlag(compiler.Flag.MUTABLE)
				val rhsTrees = evaluateImpl(AST.rhs, classDefs, topLevelGoingForward)
				// The Scala AST does not type it, but you can only have an expression as a rhs of a ValDef
				// It's possible that we do not get any tree if we encounter something like a ??? that we do not need to evaluate in ordre to give a meaninfull result
				rhsTrees.headOption.map { rhsTree =>
					if (topLevelGoingForward) {
						val tempName = newTermName(reservedName + "_b_" + AST.name.toString)
						val wrappedTemp = ValDef(Modifiers(), tempName, TypeTree(), rhsTree)
						val asFunDef = DefDef(importer.importModifiers(AST.mods), importer.importName(AST.name).toTermName, List(), List(), importer.importTree(AST.tpt), Select(Ident(tempName), newTermName("get")))
						List(wrappedTemp, asFunDef)
					}
					else {
						List(ValDef(importer.importModifiers(AST.mods), importer.importName(AST.name).toTermName, importer.importTree(AST.tpt), rhsTree))
					}
				}.getOrElse(Nil)
			}
		}
		def evaluateAssign(AST: compiler.Assign, classDefs: Traversable[compiler.ClassDef]): Option[Assign] = {
			val rhsTrees = evaluateImpl(AST.rhs, classDefs, false)
			rhsTrees.headOption.map { rhsTree =>
				Assign(importer.importTree(AST.lhs), rhsTree)
			}
		}
		def evaluateIf(AST: compiler.If, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): Tree = {
			val thenTree = evaluateImpl(AST.thenp, classDefs, false)
			val elseTree = evaluateImpl(AST.elsep, classDefs, false)
			val condTree = evaluateImpl(AST.cond, classDefs, false)
			val ifTree = If(condTree.head, thenTree.head, elseTree.head)
			if (topLevel) wrapInTry(ifTree) else ifTree
		}
		def evaluateMatch(AST: compiler.Match, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): Tree = {
			val selectorTree = evaluateImpl(AST.selector, classDefs, false)
			val caseTrees = AST.cases.map(evaluateImpl(_, classDefs, false))
			val matchTree = Match(selectorTree.head, caseTrees.map(_.head.asInstanceOf[CaseDef]))
			if (topLevel) wrapInTry(matchTree) else matchTree
		}
		def evaluateDefDef(AST: compiler.DefDef, classDefs: Traversable[compiler.ClassDef]): Option[Tree] = {
			AST.sampleParamsOption(classDefs) map { sampleValues =>
				val rhsTrees = evaluateImpl(AST.rhs, classDefs, false)
				// The Scala AST does not type it, but you can only have an expression as a rhs of a ValDef
				val block = Block(sampleValues, rhsTrees.head)
				AST.updateAttachment(paramList(sampleValues))
				wrapInTry(block)
			}
		}
		def evaluateBlock(AST: compiler.Block, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): Tree = {
			import shapeless.syntax.typeable._
			// This block was added by the Scala compiler while desaguring a Scala feature
			// For instance, when using an operator ending with a colon
			if (AST.children(0).cast[compiler.ValDef].map(_.mods.hasFlag(compiler.Flag.SYNTHETIC)).getOrElse(false) && AST.children.length == 2) {
				val trees = evaluateImpl(AST.children(1), classDefs, false)
				// We return the same compiler generated block exept we instrumented
				// the expression the user is interested in
				// i.e we did not instrument the compiler generated code that would surprise the user
				Block(List(importer.importTree(AST.children(0))), trees.head)
			}
			else {
				val additionnalClassDefs = AST.children.collect { case child: compiler.ClassDef => child }
				val trees = evaluateList(AST.children, classDefs, false)
				val newBlock = Block(trees.init, trees.last)
				if (topLevel) wrapInTry(newBlock) else newBlock
			}
		}
		def evaluateList(list: List[compiler.Tree], classDefs: Traversable[compiler.ClassDef], topLevel: Boolean) : List[Tree] = {
			val additionnalClassDefs = list.collect { case child: compiler.ClassDef => child }
			val childTrees = list.map(evaluateImpl(_, classDefs ++ additionnalClassDefs, topLevel))
			childTrees.flatten
		}
		def evaluateClassDef(AST: compiler.ClassDef, classDefs: Traversable[compiler.ClassDef]): Option[Block] = {
			AST.constructorOption.flatMap { constructor =>
				constructor.sampleParamsOption(classDefs).map { sampleParams =>
					AST.updateAttachment(paramList(sampleParams))
					// We remove the value defintions resulting from the class parameters and the primary constructor
					val noSynthetic = AST.impl.body.filter {
						case valDef: compiler.ValDef => !sampleParams.exists( sampleValDef => sampleValDef.name == valDef.name)
						case other => other != constructor
					}
					val abstractMembers = noSynthetic.collect { case def_ : compiler.ValOrDefDef if def_.rhs.isEmpty => def_ }
					val implAbstractMembersOptions = abstractMembers.implementAbstractMembers(classDefs, createDefaultSamplePool)

					if (implAbstractMembersOptions.contains(None)) None
					else {
						val implementedMembers = implAbstractMembersOptions.flatten
						val noAbstract = noSynthetic.filter {
							case valDef: compiler.ValOrDefDef => !implementedMembers.exists( sampleValDef => sampleValDef.name == valDef.name)
							case other => other != constructor
						}
						val sampleValues = sampleParams ++ implementedMembers
						val trees = evaluateList(noAbstract, classDefs, false)
						if (trees.isEmpty) None else Some(Block(sampleValues ++ trees, Literal(Constant(()))))
					}
				}
			}.flatten
		}
		def evaluateModuleDef(AST: compiler.ModuleDef, classDefs: Traversable[compiler.ClassDef]): Block = {
			val body = AST.impl.body.filter(!isConstructor(_))
			val trees = evaluateList(body, classDefs, false)
			Block(trees, Literal(Constant(())))
		}
		def evaluateImpl(AST: compiler.Tree, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): List[Tree] = {
			AST match {
				case assign: compiler.Assign => evaluateAssign(assign, classDefs).toList
				case ifTree: compiler.If => List(evaluateIf(ifTree, classDefs, topLevel))
				case matchTree: compiler.Match => List(evaluateMatch(matchTree, classDefs, topLevel))
				case caseDef: compiler.CaseDef => {
					val newBody = evaluateImpl(caseDef.body, classDefs, false)
					List(CaseDef(importer.importTree(caseDef.pat), newBody.head))
				}
				case valDef: compiler.ValDef => evaluateValDef(valDef, classDefs, topLevel).toList
				case funDef: compiler.DefDef => {
					val block = evaluateDefDef(funDef, classDefs)
					importer.importTree(funDef) :: block.toList
				}
				case block: compiler.Block => {
					val newblock = evaluateBlock(block, classDefs, topLevel)
					List(newblock)
				}
				case classDef: compiler.ClassDef => {
					val block = evaluateClassDef(classDef, classDefs)
					importer.importTree(classDef) :: block.toList
				}
				case moduleDef: compiler.ModuleDef => {
					val block = evaluateModuleDef(moduleDef, classDefs)
					List(importer.importTree(moduleDef), block)
				}
				case import_ : compiler.Import => List(importer.importTree(import_))
				case compiler.EmptyTree => Nil
				case expr => {
					val steps = Nil
					// This is where most of the magic happens!
					// We change the user's code to store evaluated expressions
					// before moving on with the rest of the program

					// We create a temporary value to store the result of the expression
					// We wrap the rhs in a Try monad to detect exceptions
					val tempName = newTermName(nameOfTemp)
					val tempVal = ValDef(Modifiers(), tempName, TypeTree(), wrapInTry(importer.importTree(expr)))
					val indexLiteral = Literal(Constant(index))
					val storeInMap = Apply(Select(Ident(newTermName(nameOfMap)), newTermName("update")), List(indexLiteral, Ident(tempName)))
					// We extract the actual value from the monad to preserve original program flow
					val ref = Ident(tempName)
					val extract = Select(ref, newTermName("get"))
					val lastExpr = if (topLevel) ref else extract
					val evalAndStore = Block(List(tempVal, storeInMap), lastExpr)
					val valueResult = PlaceHolder(index)
					index = index + 1
					expr.updateAttachment(SimpleExpressionResult(final_ = valueResult, steps = steps))
					List(evalAndStore)
				}
			}
		}

		val beginMap =  ValDef(Modifiers(), newTermName(nameOfMap), TypeTree(), Apply(TypeApply(Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("mutable")), newTermName("Map")), List(Ident(newTypeName("Int")), Ident(newTypeName("Any")))), List()))
		val retrieveMap = Ident(newTermName(nameOfMap))

		val trees = evaluateList(AST, Nil, topLevel = true)

		val transformedUserAST = wrapInTry(Block(trees, reify{ () }.tree))
		val transformedAST = Block(List(beginMap, transformedUserAST), retrieveMap)

		transformedAST
	}

	def computeResults(code: String, enableSteps: Boolean = true): Result = {
		val code1 = """object Codebrew {
                 						 |	4 + 4
                 						 |	gghh
                 						 |}""".stripMargin

		val file = new BatchSourceFile("default", code1)
		val response = new Response[Unit]()
		compiler.askReload(List(file), response)
		response.get // block until the presentation reloaded the code

		val response1 = new Response[compiler.Tree]()
		compiler.askLoadedTyped(file, response1)
		val AST: compiler.Tree = response1.get match {
		  case Left(tree) => tree
		  case Right(_) => error("no tree")
		}
		val actualAST = AST.asInstanceOf[compiler.ModuleDef].impl.body
		val toolBox = cm.mkToolBox()
		val instrumented = analyseAndTransform(actualAST, enableSteps = enableSteps)
		val outputStream = new java.io.ByteArrayOutputStream()
		import scala.util.{Try, Success}
		val values = Console.withOut(outputStream) {
			toolBox.eval(instrumented).asInstanceOf[scala.collection.mutable.Map[Int, Try[Any]]]
		}
		/*
		// Take the results that were statically generated and update them now that we have the map that tells us
		// what happened at runtime.
		val statementResults = statementResultsWithPlaceHolders.map(_.transform {
			// Extract values from the map and insert them in SimpleExpressionResults
			case simple @ SimpleExpressionResult(PlaceHolder(id), _, _) =>
				values.get(id).map { value =>
					simple.copy(final_ = value.transform(
						value => Success(ObjectResult(value)),
						exception => Success(exception match {
							case ex: scala.NotImplementedError => NotImplementedResult
							case ex => ExceptionValue(exception)
						})
					).get)
				}.getOrElse(simple) // If no value was found, just return the structure unchanged,
									// it will probably get eliminated by the IfThenElse substitution
			// We do not know in advance which branch will be executed, but now we know, so update the structure to reflect this
			case result @IfThenElseResultPlaceholder(cond, then, else_, line) => {
				val executed = if (result.isTrue) then else else_
				IfThenElseResult(cond, executed, line)
			}
			case result @MatchResultPlaceholder(selector, cases, line) =>
				MatchResult(selector, cases.find(_.wasEvaluated).get, line)
			case block @BlockResult(children, _) => block.copy(children = children.filter(_.wasEvaluated))
		})
		*/
		Result(actualAST, outputStream.toString)
	}

	implicit class AugmentedClassDef(classDef: compiler.ClassDef) {
		val abstractMembers = classDef.impl.body.collect { case def_ : compiler.ValOrDefDef if def_.rhs.isEmpty => def_ }
		val constructorOption = classDef.impl.body.find(isConstructor).asInstanceOf[Option[compiler.DefDef]]
		val isTrait = classDef.mods.hasFlag(compiler.Flag.INTERFACE) && classDef.mods.hasFlag(compiler.Flag.ABSTRACT)
		val isCaseClass = classDef.mods.hasFlag(compiler.Flag.CASE)
	}

	def isConstructor(tree: compiler.Tree) = tree match {
		case defdef: compiler.DefDef => defdef.name == nme.CONSTRUCTOR
		case _ => false
	}

	implicit class DefDefWithSamples(defdef: compiler.DefDef) {
		def sampleParamsOption(classDefs: Traversable[compiler.ClassDef] = Nil, samplePool: SamplePool = createDefaultSamplePool): Option[List[ValDef]] = {
			def sequentialEagerTerminationImpl(valDefs: List[compiler.ValDef]): Option[List[ValDef]] = {
				if (valDefs.isEmpty) Some(Nil)
				else {
					valDefs.head.withSampleValue(classDefs, samplePool).map { newValDef =>
						sequentialEagerTerminationImpl(valDefs.tail).map { rest => newValDef :: rest }
					}.flatten
				}
			}
			val allParams = defdef.vparamss.flatten
			sequentialEagerTerminationImpl(allParams)
		}
	}

	implicit class ValDefWithSample(valDef: compiler.ValDef) {

		def withSampleValue(classDefs: Traversable[compiler.ClassDef] = Nil, samplePool: SamplePool = createDefaultSamplePool): Option[ValDef] =
			if (valDef.rhs.isEmpty)
				valDef.sampleValue(classDefs, samplePool).map { rhs =>
					atPos(importer.importPosition(valDef.pos))(ValDef(Modifiers(), importer.importName(valDef.name).toTermName, TypeTree(), rhs))
				}
			else Some(importer.importTree(valDef).asInstanceOf[ValDef])

		def sampleValue(classDefs: Traversable[compiler.ClassDef] = Nil, samplePool: SamplePool = createDefaultSamplePool): Option[Tree] =
			valDef.tpt.sampleValue(classDefs, samplePool)
	}

	implicit class BodyWithSample(value: List[compiler.ValOrDefDef]) {
		def implementAbstractMembers(classDefs: Traversable[compiler.ClassDef] = Nil,
		                             samplePool: SamplePool = createDefaultSamplePool): List[Option[ValOrDefDef]] =
			value.map {
				case valDef: compiler.ValDef => valDef.withSampleValue(classDefs, samplePool)
				case defDef: compiler.DefDef => defDef.tpt.sampleValue(classDefs, samplePool).map { sampleImpl =>
					DefDef(Modifiers(), importer.importName(defDef.name).toTermName, List(), defDef.vparamss.map( params => params.map(importer.importTree(_).asInstanceOf[ValDef])), TypeTree(), sampleImpl)
				}
			}
	}

	implicit class TreeWithSample(tree: compiler.Tree) {

		def sampleValue(classDefs: Traversable[compiler.ClassDef] = Nil,
                        samplePool: SamplePool = createDefaultSamplePool): Option[Tree] = {

			def assignValueOfCustomType: Option[Tree] = {
				val concretePred = (classDef: compiler.ClassDef) => {
					val isSameConcreteClass = classDef.name.toString == tree.toString && !classDef.mods.hasFlag(compiler.Flag.ABSTRACT)
					val isSubClass = classDef.impl.parents.exists( (parent: compiler.Tree) => { parent.toString == tree.toString})
					isSameConcreteClass || isSubClass
				}
				val abstractPred = (classDef: compiler.ClassDef) => {
					classDef.name.toString == tree.toString && classDef.mods.hasFlag(compiler.Flag.ABSTRACT)
				}
				// TODO: Consider possibility that an object could extend an abstract type and be of use here
				classDefs.find(concretePred).orElse(classDefs.find(abstractPred)).flatMap { classDef =>
					def assignAnonClass: Option[Tree] = {
						val implementedMembersOption: List[Option[ValOrDefDef]] =
							classDef.abstractMembers.implementAbstractMembers(classDefs, samplePool)
							if (implementedMembersOption.contains(None)) None
							else {
								val implementedMembers = implementedMembersOption.flatten
								Some(anonClass(classDef.name.toString, implementedMembers))
							}
					}
					if (classDef.isTrait) assignAnonClass
					else classDef.constructorOption.flatMap { constructorDef =>
						constructorDef.sampleParamsOption(classDefs, samplePool).flatMap { innerValues =>
							if (classDef.mods.hasFlag(compiler.Flag.ABSTRACT)) {
								// TODO: Handle case where abstract class takes parameters
								assignAnonClass
							}
							else if (classDef.isCaseClass)
								Some(Apply(Ident(newTermName(classDef.name.toString)), innerValues.map(_.rhs)))
							else
								Some(Apply(Select(New(Ident(newTypeName(classDef.name.toString))), nme.CONSTRUCTOR), innerValues.map(_.rhs)))
						}
					}
				}
			}
			tree match {
				case tpt: compiler.AppliedTypeTree => {
					val innerType = tpt.args(0)
					tpt.tpt.toString match {
						case "List" => {
							val length = samplePool.nextSeqLength
							if (length == 0)
								Some(Ident(newTermName("Nil")))
							else {
								val innerSamplesOpt = (0 until length).map{ useless => innerType.sampleValue(classDefs, samplePool)}
								if (innerSamplesOpt.exists(_.isEmpty)) None
								else {
									val innerSamples = innerSamplesOpt.map(_.get)
									val tree = Apply(Ident(newTermName("List")), innerSamples.toList)
									Some(tree)
								}
							}
						}
						case "Option" => {
							val isSome = samplePool.nextOptionIsSome
							if (isSome)
								innerType.sampleValue(classDefs, samplePool).map { innerValue =>
									Apply(Ident(newTermName("Some")), List(innerValue))
								}
							else
								Some(Ident(newTermName("None")))
							}
						case "Seq" => {
							val length = samplePool.nextSeqLength
							if (length == 0)
								Some(Ident(newTermName("Nil")))
							else {
								val innerSamplesOpt = (0 until length).map(useless => innerType.sampleValue(classDefs, samplePool))
								if (innerSamplesOpt.exists(_.isEmpty)) None
								else {
									val innerSamples = innerSamplesOpt.map(_.get)
									val tree = Apply(Ident(newTermName("Seq")), innerSamples.toList)
									Some(tree)
								}
							}
						}
						case _ => None
					}
				}
				case tpt: compiler.ExistentialTypeTree => {
					// TODO: Remove hard coding for only one type param
					// Probably fixed by removing the 0 index retrieval and the list construction
					val upperBound = tpt.whereClauses(0).asInstanceOf[compiler.TypeDef].rhs.asInstanceOf[compiler.TypeBoundsTree].hi
					// Let's exract the AppliedTypeTree from this Existential and replace the type argument
					// with the upper bound of the where clause
					val innerType = tpt.tpt.asInstanceOf[compiler.AppliedTypeTree]
					val simplified = compiler.AppliedTypeTree(innerType.tpt, List(upperBound))
					simplified.sampleValue(classDefs, samplePool)
				}
				case tpt: compiler.Select => samplePool.get(tpt.name.toString).orElse(assignValueOfCustomType)
				case tpt => samplePool.get(tpt.toString).orElse(assignValueOfCustomType)
			}
		}
	}

	def paramList(valDefs: List[ValDef]): List[AssignOrNamedArg] = {
		valDefs.map(valDef => AssignOrNamedArg(Ident(valDef.name), valDef.rhs))
	}

	// I don't like the fact that 2 + 3 = 5 which is why I'd rather
	// start the sample Int values at 3 instead of at 2 in the primeNumbers list

	val sampleIntValues = Math.primeNumbers.drop(1).map(_.toString)
	val sampleStringValues = "\"foo\"" #:: "\"bar\"" #:: "\"biz\"" #:: "\"says\"" #:: "\"Hello\"" #:: "\"World\"" #:: "\"bazinga\"" #:: Stream.empty repeat 
	val alphabet = "abcdefghijklmnopqrstuvwxyz"
	val sampleCharValues = (0 until alphabet.length).map(alphabet.charAt(_)).toStream.repeat.map("\'" + _ + "\'")
	val sampleDoubleValues = Math.primeNumbers.map(_ - 0.5).map(_.toString)
	val sampleFloatValues = sampleDoubleValues.map(_ + "f")
	val sampleBooleanValues = (true #:: false #:: Stream.empty).repeat.map(_.toString)
	//val sampleAnyValValues = sampleIntValues.zip5(sampleStringValues, sampleFloatValues, sampleBooleanValues, sampleCharValues).flatten(_.productIterator)
	val sampleAnyValValues = "3" #:: "\'f\'" #:: "true" #:: Stream.empty repeat
	val sampleAnyValues = "3" #:: "\"foo\"" #:: "true" #:: Stream.empty repeat
	val sampleAnyRefValues = "\"foo\"" #:: "List(3,5,7)" #:: "Some(5)" #:: Stream.empty repeat

	val sampleSeqLengths = 3 #:: 0 #:: 1 #:: 2 #:: Stream.empty repeat
	val sampleSomeOrNone = true #:: false #:: Stream.empty repeat

	val simpleValues = Map(
		"Int" -> sampleIntValues,
		"String" -> sampleStringValues,
		"Float" -> sampleFloatValues,
		"Boolean" -> sampleBooleanValues,
		"Long" -> sampleIntValues,
		"Double" -> sampleDoubleValues,
		"Byte" -> sampleIntValues,
		"Short" -> sampleIntValues,
		"Char" -> sampleCharValues,
		"AnyVal" -> sampleAnyValValues,
		"Any" -> sampleAnyValues,
		"AnyRef" -> sampleAnyRefValues
	)

	val parsedSimpleValues = simpleValues.mapValues(stream => stream.map(cm.mkToolBox().parse(_)))
	def createDefaultSamplePool = new SamplePool(parsedSimpleValues, sampleSeqLengths, sampleSomeOrNone)
}

class SamplePool(var values: Map[String, Stream[Tree]], var seqLengths: Stream[Int], var someOrNone: Stream[Boolean]) {

	def get(type_ : String): Option[Tree] =
		values.get(type_).map { stream => // TODO: Remove the race condition here that might cause problems in the future with parallelization
			values = values.updated(type_, stream.tail)
			stream.head
		}

	def nextSeqLength: Int = {
		val result = seqLengths.head
		seqLengths = seqLengths.tail
		result
	}

	def nextOptionIsSome: Boolean = {
		val result = someOrNone.head
		someOrNone = someOrNone.tail
		result
	}
}