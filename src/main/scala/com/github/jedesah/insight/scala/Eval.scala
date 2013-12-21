package com.github.jedesah.insight.scala

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.{universe => ru}
import ru._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe.Flag._

import com.github.jedesah.Math
import com.github.jedesah.ScalaUtils._
import com.github.jedesah.AugmentedCollections._
import TreeExtras._

import scala.reflect.internal.util._
import scala.tools.nsc.interactive.Response

object Eval {

	val reporter = new scala.tools.nsc.reporters.StoreReporter()
	val settings = new scala.tools.nsc.Settings()
	settings.classpath.value = System.getProperty("sbt.application.class.path")
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

	def instrument(ASTs: List[compiler.Tree], enableSteps: Boolean): Tree = {

		var index = 0

		val tryTerm = Select(Select(Ident(newTermName("scala")), newTermName("util")), newTermName("Try"))
		val tryType = Select(Select(Ident(newTermName("scala")), newTermName("util")), newTypeName("Try"))
		val mutableMapTerm = Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("mutable")), newTermName("Map"))
		def wrapInTry(toWrap: Tree): Tree = Apply(tryTerm, List(toWrap))

		object Traverse {
			def instrumentValDef(AST: compiler.ValDef, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean): List[Tree] = {
				if (AST.mods.hasFlag(compiler.Flag.SYNTHETIC)) List(importer.importTree(AST))
				else {
					val topLevelGoingForward = topLevel && !AST.mods.hasFlag(compiler.Flag.MUTABLE)
					val declaredType = if (AST.tpt.isEmpty) None else Some(importer.importTree(AST.tpt))
					val rhsTrees = instrument(AST.rhs, classDefs, false, topLevelGoingForward, declaredType)
					// The Scala AST does not type it, but you can only have one expression as a rhs of a ValDef
					rhsTrees.headOption.map { rhsTree =>
						if (topLevelGoingForward) {
							// If we are at the top level, we try and be smart and apply a transformation to the AST in order
							// to keep on going even if a runtime error is thrown and uncaught while computing the value of the definition
							// We wrap the actual value in a Try and give it another name and then create a function definition with the
							// same name as the value definition. The subsequent user code will (without knowing it) call the function instead
							// of the value (which is now a value of type Try). The function calls get on the value of the Try.
							// This means any subsequent code that uses the value definition which threw an exception will also throw an exception
							// but any code that nothing to do with this value will go on unaffected.
							// The semantics of this does not hold anywhere else than at the topLevel where this is a desired feature.
							// It would not make much to behave like this within a Block
							val tempName = newTermName(reservedName + "_b_" + AST.name.toString)
							val wrappedTemp = {
								// Previously, we put no type and the compiler did the job of inferring that the return type was an instance of Try for us
								// However this caused some bugs with user code with declared types that affect type inference
								// So we put the same type as the user but within a Try[‘userType‘]
								val typeDeclaration = if (AST.tpt.isEmpty) importer.importTree(AST.tpt) // But we can't do that if we don't know the type yet, but there is no problem in that case
							                          else AppliedTypeTree(tryType, List(importer.importTree(AST.tpt)))
								ValDef(Modifiers(), tempName, typeDeclaration, rhsTree)
							}
							val asFunDef = DefDef(importer.importModifiers(AST.mods), importer.importName(AST.name).toTermName, List(), List(), importer.importTree(AST.tpt), Select(Ident(tempName), newTermName("get")))
							List(wrappedTemp, asFunDef)
						}
						else {
							List(ValDef(importer.importModifiers(AST.mods), importer.importName(AST.name).toTermName, importer.importTree(AST.tpt), rhsTree))
						}
					}.getOrElse(Nil)
				}
			}
			def instrumentAssign(AST: compiler.Assign, classDefs: Traversable[compiler.ClassDef]): Option[Assign] = {
				val rhsTrees = instrument(AST.rhs, classDefs, false)
				rhsTrees.headOption.map { rhsTree =>
					Assign(importer.importTree(AST.lhs), rhsTree)
				}
			}
			def instrumentIf(AST: compiler.If, classDefs: Traversable[compiler.ClassDef], declaredType: Option[Tree]): Tree = {
				val condTree = instrument(AST.cond, classDefs, false)
				val thenTree = instrument(AST.thenp, classDefs, false, false, declaredType)
				val elseTree = instrument(AST.elsep, classDefs, false, false, declaredType)
				If(condTree.head, thenTree.head, elseTree.head)
			}
			def instrumentMatch(AST: compiler.Match, classDefs: Traversable[compiler.ClassDef], declaredType: Option[Tree]): Tree = {
				val selector = instrument(AST.selector, classDefs, false)
				val caseTrees = AST.cases.map(instrument(_, classDefs, false, false, declaredType))
				Match(selector.head, caseTrees.map(_.head.asInstanceOf[CaseDef]))
			}
			def instrumentDefDef(AST: compiler.DefDef, classDefs: Traversable[compiler.ClassDef]): Option[Tree] = {
				AST.sampleParamsOption(classDefs) map { sampleValues =>
					val declaredType = if (AST.tpt.isEmpty) None else Some(importer.importTree(AST.tpt))
					val rhsTrees = instrument(AST.rhs, classDefs, false, false, declaredType)
					// The Scala AST does not type it, but you can only have an expression as a rhs of a ValDef
					// It's possible that we do not get any tree if we encounter something like a ??? that we do not need to evaluate in ordre to give a meaninfull result
					val block = Block(sampleValues, rhsTrees.head)
					AST.updateAttachment(paramList(sampleValues))
					wrapInTry(block)
				}
			}
			def instrumentBlock(AST: compiler.Block, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean, immidiateChildOfTopLevelValDef: Boolean, declaredType: Option[Tree]): Tree = {
				import shapeless.syntax.typeable._
				// This block was added by the Scala compiler while desaguring a Scala feature
				// For instance, when using an operator ending with a colon
				val isSynthetic = AST.children(0).cast[compiler.ValDef].map(_.mods.hasFlag(compiler.Flag.SYNTHETIC)).getOrElse(false) && AST.children.length == 2
				if (isSynthetic) {
					// Okay then, let's just instrument this block as a holistic value
					instrumentSimpleExpression(AST, topLevel, immidiateChildOfTopLevelValDef, declaredType)
				}
				else {
					val trees = instrumentList(AST.children, classDefs, false, declaredType)
					val newBlock = Block(trees.init, trees.last)
					if (topLevel || immidiateChildOfTopLevelValDef)
						wrapInTry(newBlock)
					else newBlock
				}
			}
			def instrumentList(list: List[compiler.Tree], classDefs: Traversable[compiler.ClassDef], topLevel: Boolean, declaredType: Option[Tree] = None): List[Tree] = {
				if (list.isEmpty) Nil
				else {
					val additionnalClassDefs = list.collect { case child: compiler.ClassDef => child }
					def eval(tree: compiler.Tree, declaredType: Option[Tree] = None) = instrument(tree, classDefs ++ additionnalClassDefs, topLevel, false, declaredType)
					val childTrees = list.init.map(eval(_)) :+ eval(list.last, declaredType)
					childTrees.flatten
				}
			}
			def instrumentClassDef(AST: compiler.ClassDef, classDefs: Traversable[compiler.ClassDef]): Option[Block] = {
				AST.constructorOption.flatMap { constructor =>
					constructor.sampleParamsOption(classDefs).map { sampleParams =>
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
							val trees = instrumentList(noAbstract, classDefs, false)
							if (trees.isEmpty) None else Some(Block(sampleValues ++ trees, Literal(Constant(()))))
						}
					}
				}.flatten
			}
			def instrumentModuleDef(AST: compiler.ModuleDef, classDefs: Traversable[compiler.ClassDef]): Block = {
				val body = AST.impl.body.filter(!isConstructor(_))
				val trees = instrumentList(body, classDefs, false)
				Block(trees, Literal(Constant(())))
			}
			def instrumentSimpleExpression(AST: compiler.Tree, topLevel: Boolean, immidiateChildOfTopLevelValDef: Boolean, declaredType: Option[Tree]): Tree = {
				val steps = Nil
				// This is where most of the magic happens!
				// We change the user's code to store evaluated expressions
				// before moving on with the rest of the program

				// We create a temporary value to store the result of the expression
				// We wrap the rhs in a Try monad to detect exceptions
				val indexLiteral = Literal(Constant(index))
				val newExpr = wrapInTry(importer.importTree(AST))
				val wholeThing =
					if (topLevel) {
						val storeInMap = Apply(Select(Ident(newTermName(nameOfMap)), newTermName("update")), List(indexLiteral, newExpr))
						storeInMap
					}
					else {
						val tempName = newTermName(nameOfTemp)
						val tempVal = {
							val tempDeclaredType = declaredType.map(declaredType => AppliedTypeTree(tryType, List(declaredType))).getOrElse(TypeTree())
							ValDef(Modifiers(), tempName, tempDeclaredType, newExpr)
						}
						val storeInMap = Apply(Select(Ident(newTermName(nameOfMap)), newTermName("update")), List(indexLiteral, Ident(tempName)))
						// We extract the actual value from the monad to preserve original program flow
						val ref = Ident(tempName)
						val extract = Select(ref, newTermName("get"))
						val lastExpr = if (immidiateChildOfTopLevelValDef) ref else extract
						val evalAndStore = Block(List(tempVal, storeInMap), lastExpr)
						evalAndStore
					}
				val valueResult = PlaceHolder(index)
				index = index + 1
				AST.updateAttachment(SimpleExpressionResult(final_ = valueResult, steps = steps))
				wholeThing
			}
			/**
			 * @param declaredType In order to maintain the exact semantics of the code, we need to know if we are within a value or function definition and what type it
			 *                     was declared as. Of course it is also possible that it was not declared. The param should be None if it was not declared or if we are
			 *                     not within a value of function definition. Other it should contain the declared type in the form of a TypeTree
			 */
			def instrument(AST: compiler.Tree, classDefs: Traversable[compiler.ClassDef], topLevel: Boolean, immidiateChildOfTopLevelValDef: Boolean = false, declaredType: Option[Tree] = None): List[Tree] = {
				// We only need to consider the declaredType in the case of an expression that can be found on the rhs of a definition
				// This means we need to consider it for If, Match, Block and any other "simple" expression
				AST match {
					case assign: compiler.Assign => instrumentAssign(assign, classDefs).toList
					case caseDef: compiler.CaseDef => {
						val newBody = instrument(caseDef.body, classDefs, false)
						List(CaseDef(importer.importTree(caseDef.pat), newBody.head))
					}
					case valDef: compiler.ValDef => instrumentValDef(valDef, classDefs, topLevel).toList
					case funDef: compiler.DefDef => {
						val block = instrumentDefDef(funDef, classDefs)
						importer.importTree(funDef) :: block.toList
					}
					case classDef: compiler.ClassDef => {
						val block = instrumentClassDef(classDef, classDefs)
						importer.importTree(classDef) :: block.toList
					}
					case moduleDef: compiler.ModuleDef => {
						val block = instrumentModuleDef(moduleDef, classDefs)
						List(importer.importTree(moduleDef), block)
					}
					case import_ : compiler.Import => List(importer.importTree(import_))
					case compiler.EmptyTree => Nil
					case expr => {
						expr match {
							case _ : compiler.If | _ : compiler.Match => {
								val newExpression = expr match {
									// This must be done because Scala sucks sometimes (or maybe I suck).
									// More precisely, there are some weird restrictions to overloading
									// functions that are defined within other functions.
									case ifTree: compiler.If => instrumentIf(ifTree, classDefs, declaredType)
									case matchTree: compiler.Match => instrumentMatch(matchTree, classDefs, declaredType)
								}
								val safeExpression = if (topLevel || immidiateChildOfTopLevelValDef) wrapInTry(newExpression)
								                     else newExpression
								List(safeExpression)
							}
							// We need to make a special case for Block because it might be synthetic (I know...)
							case block: compiler.Block => {
								val newTree = instrumentBlock(block, classDefs, topLevel, immidiateChildOfTopLevelValDef, declaredType)
								List(newTree)
							}
							case simpleExpression => {
								val instrumented = instrumentSimpleExpression(simpleExpression, topLevel, immidiateChildOfTopLevelValDef, declaredType)
								List(instrumented)
							}
						}
					}
				}
			}
		}

		val declareValuesMap = ValDef(Modifiers(), newTermName(nameOfMap), TypeTree(), Apply(TypeApply(mutableMapTerm, List(Ident(newTypeName("Int")), Ident(newTypeName("Any")))), List()))
		val retrieveMap = Ident(newTermName(nameOfMap))

		val trees = Traverse.instrumentList(ASTs, Nil, topLevel = true)

		val instrumentedUserAST = wrapInTry(Block(trees, reify{ () }.tree))
		val instrumentedAST = Block(List(declareValuesMap, instrumentedUserAST), retrieveMap)

		instrumentedAST
	}

	def parse(code: String): List[compiler.Tree] = {
		if (code == "") Nil
		else {
			val prefix = "object Codebrew {"
			val suffix = "}"

			val file = new BatchSourceFile("default", prefix + code + suffix)
			val response = new Response[Unit]()
			compiler.askReload(List(file), response)
			response.get // block until the presentation reloaded the code

			val response1 = new Response[compiler.Tree]()
			compiler.askLoadedTyped(file, response1)
			val AST: compiler.Tree = response1.get match {
			  case Left(tree) => tree
			  case Right(_) => error("no tree")
			}
			// The AST produced by the PC is contained in an "empty" package
			// Then we need to extract the list of ASTs from the object we wrapped them in.
			AST.children(1).asInstanceOf[compiler.ModuleDef].impl.body.drop(1)
		}
	}

	def eval(code: String): Result = {
		eval(parse(code), true)
	}
	def eval(ASTs: List[compiler.Tree], enableSteps: Boolean = true) = {
		val instrumented = instrument(ASTs, enableSteps = enableSteps)

		val outputStream = new java.io.ByteArrayOutputStream()
		import scala.util.{Try, Success}
		val toolBox = cm.mkToolBox()
		val toEval = toolBox.resetAllAttrs(instrumented.duplicate)
		val values = Console.withOut(outputStream) {
			toolBox.eval(toEval).asInstanceOf[scala.collection.mutable.Map[Int, Try[Any]]]
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
		Result(ASTs, outputStream.toString)
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