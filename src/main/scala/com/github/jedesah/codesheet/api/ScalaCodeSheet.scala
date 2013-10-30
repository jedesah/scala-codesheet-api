package com.github.jedesah.codesheet.api

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe.Flag._

import com.github.jedesah.Math
import com.github.jedesah.ScalaUtils._
import com.github.jedesah.AugmentedCollections._

object ScalaCodeSheet {

	val nameOfMap = "youcannotnameyourvariablelikethisincodesheet_map"
	val nameOfTemp = "youcannotnameyourvariablelikethisincodesheet_temp"

	def childrenRepr(at: Int, elems: List[{ def line: Int; def userRepr: String}]): String =
		elems.sortBy(_.line).foldLeft((at, "")) { case ((at, result), child) =>
			val newResult =
				if (at == child.line && result != "") result + "; " + child.userRepr
				else result + "\n" * (child.line - at) + child.userRepr
			(child.line, newResult)
		}._2

	/*def willNeedLater = { val singleChild = childrenRepr(at, children)
				if (singleChild.isEmpty) ""
				else if (!singleChild.contains("\n")) " " + singleChild
				else singleChild.tabulate}*/

	case class Result(subResults: List[StatementResult], output: String) {
		def userRepr = childrenRepr(1, subResults.asInstanceOf[List[{ def line: Int; def userRepr: String}]])
	}

	abstract class StatementResult(val line: Int) {
		def userRepr: String
		protected def asParamList(params: List[AssignOrNamedArg]) = params.mkStringNoEndsIfEmpty("(", ", ", ")")
		def transform(pf: PartialFunction[StatementResult, StatementResult]): StatementResult
	}
	abstract class ExpressionResult(override val line: Int) extends StatementResult(line) {
		def transform(pf: PartialFunction[StatementResult, StatementResult]): ExpressionResult
	}
	case class BlockResult(children: List[StatementResult], override val line: Int) extends ExpressionResult(line) {
		def userRepr = userRepr(Nil)
		def userRepr(implementedMembers: List[ValOrDefDef]): String = {
			case class WrappedValOrDefDef(valOrDefDef: ValOrDefDef) {
				def userRepr = valOrDefDef.toString
				def line = valOrDefDef.pos.line
			}
			val elemsToPrint = (children ++ implementedMembers.map(WrappedValOrDefDef(_))).asInstanceOf[List[{def line: Int; def userRepr: String}]]
			if (elemsToPrint.isEmpty) ""
			else {
				val childRepr = childrenRepr(line, elemsToPrint)
				if (childRepr.contains("\n")) "{" + childRepr.lines.toList.head + "\n" + childRepr.lines.toList.tail.map(_.tabulate).mkString("\n") + "\n}"
				else s"{$childRepr}"
			}
		}
		def transform(pf: PartialFunction[StatementResult, StatementResult]): BlockResult = {
			val sub = transformChildren(pf)
			pf.applyOrElse(sub, identity[StatementResult]).asInstanceOf[BlockResult]
		}
		def transformChildren(pf: PartialFunction[StatementResult, StatementResult]) = copy(children = children.map(_.transform(pf)))
	}
	case class ValDefResult(name: String, inferredType: Option[String], rhs: ExpressionResult, override val line: Int) extends StatementResult(line) {
		def userRepr = {
			val lineDiff = rhs.line - line
			name + inferredType.map(": " + _).getOrElse("") + " =" + "\n" * lineDiff + (if(lineDiff > 0) "\t" else " ") + rhs.userRepr
		}
		def transform(pf: PartialFunction[StatementResult, StatementResult]): ValDefResult = {
			val sub = transformChildren(pf)
			pf.applyOrElse(sub, identity[StatementResult]).asInstanceOf[ValDefResult]
		}
		def transformChildren(pf: PartialFunction[StatementResult, StatementResult]) = copy(rhs = rhs.transform(pf))
	}
	case class DefDefResult(name: String, params: List[AssignOrNamedArg], inferredType: Option[String], rhs: ExpressionResult, override val line: Int) extends StatementResult(line) {
		def userRepr = name + asParamList(params) + inferredType.map(": " + _).getOrElse("") + " => " + rhs.userRepr
		def transform(pf: PartialFunction[StatementResult, StatementResult]): DefDefResult = {
			val sub = transformChildren(pf)
			pf.applyOrElse(sub, identity[StatementResult]).asInstanceOf[DefDefResult]
		}
		def transformChildren(pf: PartialFunction[StatementResult, StatementResult]) = copy(rhs = rhs.transform(pf))
	}
	class ClassDefResult(val name: String, val params: List[AssignOrNamedArg], val body: BlockResult, override val line: Int) extends StatementResult(line) {
		def bodyRepr = body.userRepr
		def userRepr = "class " + name + asParamList(params) + (if (bodyRepr.isEmpty) "" else " " + bodyRepr)
		override def equals(other: Any) = other match { case classDefResult: ClassDefResult => equals(classDefResult) case _ => false }
		def equals(classDefResult: ClassDefResult) = name == classDefResult.name && params == classDefResult.params && body == classDefResult.body && line == classDefResult.line
		def transform(pf: PartialFunction[StatementResult, StatementResult]): ClassDefResult = {
			val sub = transformChildren(pf)
			pf.applyOrElse(sub, identity[StatementResult]).asInstanceOf[ClassDefResult]
		}
		def transformChildren(pf: PartialFunction[StatementResult, StatementResult]) = copy(body = body.transform(pf))
		def copy(name: String = name, params: List[AssignOrNamedArg] = params, body: BlockResult = body, line: Int = line) = ClassDefResult(name, params, body, line)
	}
	object ClassDefResult {
		def apply(name: String, params: List[AssignOrNamedArg], body: BlockResult, line: Int) = new ClassDefResult(name, params, body, line)
		def unapply(classDefResult: ClassDefResult) = Some((classDefResult.name, classDefResult.params, classDefResult.body, classDefResult.line))
	}
	case class AbstractClassDefResult(abstractMembers: List[ValOrDefDef], override val name: String, override val params: List[AssignOrNamedArg], override val body: BlockResult, override val line: Int)
	  extends ClassDefResult(name, params, body, line) {
		override def bodyRepr = body.userRepr(abstractMembers)
		override def userRepr = "abstract " + super.userRepr
	}
	case class ModuleDefResult(name: String, body: BlockResult, override val line: Int) extends StatementResult(line) {
		def userRepr = {
			val bodyString = body.userRepr
			if (bodyString.isEmpty) "" else name + " " + bodyString
		}
		def transform(pf: PartialFunction[StatementResult, StatementResult]): ModuleDefResult = {
			val sub = transformChildren(pf)
			pf.applyOrElse(sub, identity[StatementResult]).asInstanceOf[ModuleDefResult]
		}
		def transformChildren(pf: PartialFunction[StatementResult, StatementResult]) = copy(body = body.transform(pf))
	}
	case class SimpleExpressionResult(final_ : ValueResult, steps: List[Tree] = Nil, override val line: Int) extends ExpressionResult(line) {
		def userRepr = (steps.map(_.prettyPrint) :+ final_.userRepr).mkString(" => ")
		def transform(pf: PartialFunction[StatementResult, StatementResult]): SimpleExpressionResult = {
			pf.applyOrElse(this, identity[StatementResult]).asInstanceOf[SimpleExpressionResult]
		}
	}
	case class CompileErrorResult(message: String, override val line: Int) extends StatementResult(line) {
		def userRepr = message
		def transform(pf: PartialFunction[StatementResult, StatementResult]): CompileErrorResult = {
			pf.applyOrElse(this, identity[StatementResult]).asInstanceOf[CompileErrorResult]
		}
	}
	trait ValueResult {
		def userRepr: String
	}
	case class ExceptionResult(ex: Exception) extends ValueResult {
		def userRepr = "throws " + ex
	}
	case object NotImplementedResult extends ValueResult {
		def userRepr = "???"
	}
	case class ObjectResult(value: Any) extends ValueResult {
		def userRepr = value match {
			case _: scala.runtime.BoxedUnit | scala.Unit => ""
			case _ => value.toString
		}
	}
	private case class PlaceHolder(id: Int) extends ValueResult {
		def userRepr = error("This should not be called")
	}
	implicit def createObjectResult(value: Any) = ObjectResult(value)

	val notImplSymbol = Ident(newTermName("$qmark$qmark$qmark"))

	def analyseAndTransform(AST: Tree, enableSteps: Boolean): (Tree, List[StatementResult]) = {

		var index = 0

		def evaluateValDef(AST: ValDef, classDefs: Traversable[ClassDef]): (Option[ValDef], ValDefResult) = {
			val (rhsTrees, rhsResult) = evaluateImpl(AST.rhs, classDefs)
			// The Scala AST does not type it, but you can only have an expression as a rhs of a ValDef
			// It's possible that we do not get any tree if we encounter something like a ??? that we do not need to evaluate in ordre to give a meaninfull result
			val valDef = rhsTrees.headOption.map { rhsTree =>
				ValDef(Modifiers(), AST.name, TypeTree(), rhsTree)
			}
			val result = ValDefResult(AST.name.toString, None, rhsResult.get.asInstanceOf[ExpressionResult], line = AST.pos.line)
			(valDef, result)
		}
		def evaluateDefDef(AST: DefDef, classDefs: Traversable[ClassDef]): Option[(Option[Block], DefDefResult)] = {
			AST.sampleParamsOption(classDefs) map { sampleValues =>
				val (rhsTrees, rhsResult) = evaluateImpl(AST.rhs, classDefs)
				// The Scala AST does not type it, but you can only have an expression as a rhs of a ValDef
				// It's possible that we do not get any tree if we encounter something like a ??? that we do not need to evaluate in ordre to give a meaninfull result
				val block = rhsTrees.headOption.map { rhsTree =>
					Block(sampleValues, rhsTree)
				}
				val defDefResult = DefDefResult(AST.name.toString, paramList(sampleValues), None, rhsResult.get.asInstanceOf[ExpressionResult], line = AST.pos.line)
				(block, defDefResult)
			}
		}
		def evaluateBlock(AST: Block, classDefs: Traversable[ClassDef]): (Block, BlockResult) = {
			val (trees, results) = evaluateList(AST.children, classDefs)
			(Block(trees.init, trees.last), BlockResult(results, AST.pos.line))
		}
		def evaluateList(list: List[Tree], classDefs: Traversable[ClassDef]) : (List[Tree], List[StatementResult]) = {
			val additionnalClassDefs = list.collect { case child: ClassDef => child }
			val (childTrees, childResults) = list.map(evaluateImpl(_, classDefs ++ additionnalClassDefs)).unzip
			(childTrees.flatten, childResults.flatten)
		}
		def evaluateClassDef(AST: ClassDef, classDefs: Traversable[ClassDef]): Option[(Option[Block], ClassDefResult)] = {
			AST.constructorOption.flatMap { constructor =>
				constructor.sampleParamsOption(classDefs).map { sampleParams =>
					// We remove the value defintions resulting from the class parameters and the primary constructor
					val noSynthetic = AST.impl.body.filter {
						case valDef: ValDef => !sampleParams.exists( sampleValDef => sampleValDef.name == valDef.name)
						case other => other != constructor
					}
					val abstractMembers = noSynthetic.collect { case def_ : ValOrDefDef if def_.rhs.isEmpty => def_ }
					val implAbstractMembersOptions = abstractMembers.implementAbstractMembers(classDefs, createDefaultSamplePool)

					if (implAbstractMembersOptions.contains(None)) None
					else {
						val implementedMembers = implAbstractMembersOptions.flatten
						val noAbstract = noSynthetic.filter {
							case valDef: ValOrDefDef => !implementedMembers.exists( sampleValDef => sampleValDef.name == valDef.name)
							case other => other != constructor
						}
						val sampleValues = sampleParams ++ implementedMembers
						val (trees, results) = evaluateList(noAbstract, classDefs)
						val classDefResult = ClassDefResult(AST.name.toString, paramList(sampleParams), BlockResult(results, line = AST.pos.line), line = AST.pos.line)
						val blockOption = if (trees.isEmpty) None else Some(Block(sampleValues ++ trees, Literal(Constant(()))))
						Some(blockOption, classDefResult)
					}
				}
			}.flatten
		}
		def evaluateModuleDef(AST: ModuleDef, classDefs: Traversable[ClassDef]): (Block, ModuleDefResult) = {
			val body = AST.impl.body.filter(!isConstructor(_))
			val (trees, results) = evaluateList(body, classDefs)
			(Block(trees, Literal(Constant(()))), ModuleDefResult(AST.name.toString, BlockResult(results, line = AST.pos.line), line = AST.pos.line))
		}
		def evaluateImpl(AST: Tree, classDefs: Traversable[ClassDef]): (List[Tree], Option[StatementResult]) = {
			AST match {
				case valDef: ValDef => {
					val (newValDefOption, valDefResult) = evaluateValDef(valDef, classDefs)
					(newValDefOption.toList	, Some(valDefResult))
				}
				case funDef: DefDef => {
					evaluateDefDef(funDef, classDefs).map { case (blockOption, defDefResult) =>
						(funDef :: blockOption.toList, Some(defDefResult))
					} getOrElse (List(funDef), None)
				}
				case block: Block => {
					val (newblock, blockResult) = evaluateBlock(block, classDefs)
					(List(newblock), Some(blockResult))
				}
				case classDef: ClassDef => {
					evaluateClassDef(classDef, classDefs).map { case (blockOption, classDefResult) =>
						(classDef :: blockOption.toList, Some(classDefResult))
					} getOrElse (List(classDef), None)
				}
				case moduleDef: ModuleDef => {
					val (block, moduleDefResult) = evaluateModuleDef(moduleDef, classDefs)
					(List(moduleDef, block), Some(moduleDefResult))
				}
				case EmptyTree => (Nil, None)
				case expr => {
					val steps = Nil
					val (trees, valueResult) = if (expr.equalsStructure(notImplSymbol)) (Nil, NotImplementedResult)
					else {
						val valDef = ValDef(Modifiers(), newTermName(nameOfTemp), TypeTree(), expr)
						val storeInMap = Apply(Select(Ident(newTermName(nameOfMap)), newTermName("update")), List(Literal(Constant(index)), Ident(newTermName(nameOfTemp))))
						val refA = Ident(newTermName(nameOfTemp))
						val tree = Block(List(valDef, storeInMap), refA)
						val result = PlaceHolder(index)
						index = index + 1
						(List(tree), result)
					}
					(trees, Some(SimpleExpressionResult(final_ = valueResult, steps = steps, line = AST.pos.line)))
				}
			}
		}

		val beginMap =  ValDef(Modifiers(), newTermName(nameOfMap), TypeTree(), Apply(TypeApply(Select(Select(Select(Ident(newTermName("scala")), newTermName("collection")), newTermName("mutable")), newTermName("Map")), List(Ident(newTypeName("Int")), Ident(newTypeName("Any")))), List()))
		val retrieveMap = Ident(newTermName(nameOfMap))

		val (trees, results) = AST match {
			case block: Block if block.pos == NoPosition => evaluateList(block.children, Nil)
			case _ => {
				val (trees, resultOption) = evaluateImpl(AST, Nil)
				(trees, resultOption.toList)
			}
		}

		(Block(beginMap :: trees, retrieveMap), results)
	}

	def computeResults(code: String, enableSteps: Boolean = true): Result = try {
		val toolBox = cm.mkToolBox()
		val AST = toolBox.parse(code)
		val (instrumented, statementResultsWithPlaceHolders) = analyseAndTransform(AST, enableSteps = enableSteps)
		val outputStream = new java.io.ByteArrayOutputStream()
		val placeholderValues = Console.withOut(outputStream) {
			toolBox.eval(instrumented).asInstanceOf[scala.collection.mutable.Map[Int, Any]]
		}
		val statementResults = statementResultsWithPlaceHolders.map(_.transform {
			case simple @ SimpleExpressionResult(PlaceHolder(id), _, _) => simple.copy(final_ = placeholderValues(id))
		})
		Result(statementResults, outputStream.toString)
	} catch {
		case ToolBoxError(msg, cause) => {
			val userMessage = msg.dropWhile(_ != ':').drop(2).dropWhile(char => char == ' ' || char == '\n' || char == '\t')
			val rest:Result = if (code.lines.isEmpty) Result(Nil, "") else computeResults(code.lines.drop(1).mkString)
			Result(CompileErrorResult(userMessage, 0) :: rest.subResults, rest.output)
		}
	}

	implicit class DefDefWithSamples(defdef: DefDef) {
		def sampleParamsOption(classDefs: Traversable[ClassDef] = Nil, samplePool: SamplePool = createDefaultSamplePool): Option[List[ValDef]] = {
			def sequentialEagerTerminationImpl(valDefs: List[ValDef]): Option[List[ValDef]] = {
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

	implicit class ValDefWithSample(valDef: ValDef) {

		def withSampleValue(classDefs: Traversable[ClassDef] = Nil, samplePool: SamplePool = createDefaultSamplePool): Option[ValDef] =
			if (valDef.rhs.isEmpty)
				valDef.sampleValue(classDefs, samplePool).map { rhs =>
					atPos(valDef.pos)(ValDef(Modifiers(), valDef.name, TypeTree(), rhs))
				}
			else Some(valDef)

		def sampleValue(classDefs: Traversable[ClassDef] = Nil,	samplePool: SamplePool = createDefaultSamplePool): Option[Tree] =
			valDef.tpt.sampleValue(classDefs, samplePool)
	}

	implicit class BodyWithSample(value: List[ValOrDefDef]) {
		def implementAbstractMembers(classDefs: Traversable[ClassDef] = Nil,
		                             samplePool: SamplePool = createDefaultSamplePool): List[Option[ValOrDefDef]] =
			value.map {
				case valDef: ValDef => valDef.withSampleValue(classDefs, samplePool)
				case defDef: DefDef => defDef.tpt.sampleValue(classDefs, samplePool).map { sampleImpl =>
					DefDef(Modifiers(), defDef.name, List(), defDef.vparamss, TypeTree(), sampleImpl)
				}
			}
	}

	implicit class TreeWithSample(tree: Tree) {

		def sampleValue(classDefs: Traversable[ClassDef] = Nil,
                        samplePool: SamplePool = createDefaultSamplePool): Option[Tree] = {

			def assignValueOfCustomType: Option[Tree] = {
				val concretePred = (classDef: ClassDef) => {
					val isSameConcreteClass = classDef.name.toString == tree.toString && !classDef.isAbstract
					val isSubClass = classDef.impl.parents.exists( (parent: Tree) => { parent.toString == tree.toString})
					isSameConcreteClass || isSubClass
				}
				val abstractPred = (classDef: ClassDef) => {
					classDef.name.toString == tree.toString && classDef.isAbstract
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
							if (classDef.isAbstract) {
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
				case tpt: AppliedTypeTree => {
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
				case tpt: ExistentialTypeTree => {
					// TODO: Remove hard coding for only one type param
					// Probably fixed by removing the 0 index retrieval and the list construction
					val upperBound = tpt.whereClauses(0).asInstanceOf[TypeDef].rhs.asInstanceOf[TypeBoundsTree].hi
					// Let's exract the AppliedTypeTree from this Existential and replace the type argument
					// with the upper bound of the where clause
					val innerType = tpt.tpt.asInstanceOf[AppliedTypeTree]
					val simplified = AppliedTypeTree(innerType.tpt, List(upperBound))
					simplified.sampleValue(classDefs, samplePool)
				}
				case tpt: Select => samplePool.get(tpt.name.toString).orElse(assignValueOfCustomType)
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