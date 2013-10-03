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

    case class BlockResult(children: List[Result]) {
        def userRepr = childrenRepr(1)
        def wrappedUserRepr(at: Int, forceWrap: Boolean = false): String =
            if (children.size > 1 || (forceWrap && children.size == 1))
                " {" + childrenRepr(at).tabulate + "\n}"
            else {
                val singleChild = childrenRepr(at)
                if (singleChild.isEmpty) ""
                else if (!singleChild.contains("\n")) " " + singleChild
                else singleChild.tabulate
            }
        def childrenRepr(at: Int): String = children.foldLeft((at, "")) { case ((at, result), child) =>
            val newResult =
                if (at == child.line && result != "") result + "; " + child.userRepr
                else result + "\n" * (child.line - at) + child.userRepr
            (child.line, newResult)
        }._2
    }
    abstract class Result(val line: Int) {
        def userRepr: String
        protected def userRepr(params: List[AssignOrNamedArg]) = if (params.length == 0) "" else "(" + params.mkString(", ") + ")"
    }
    case class ValDefResult(name: String, inferredType: Option[String], rhs: BlockResult, override val line: Int) extends Result(line) {
        def userRepr = name + inferredType.map(": " + _).getOrElse("") + " =" + rhs.wrappedUserRepr(line)
    }
    case class DefDefResult(name: String, params: List[AssignOrNamedArg], inferredType: Option[String], rhs: BlockResult, override val line: Int) extends Result(line) {
        def userRepr = name + userRepr(params) + inferredType.map(": " + _).getOrElse("") + " =>" + rhs.wrappedUserRepr(line)
    }
    case class ClassDefResult(name: String, params: List[AssignOrNamedArg], body: BlockResult, override val line: Int) extends Result(line) {
        def userRepr = name + userRepr(params) + body.wrappedUserRepr(line, forceWrap = true)
    }
    case class ModuleDefResult(name: String, body: BlockResult, override val line: Int) extends Result(line) {
        def userRepr = {
            val bodyString = body.wrappedUserRepr(line, forceWrap = true)
            if (bodyString.isEmpty) "" else name + bodyString
        }
    }
    case class ExpressionResult(final_ : ValueResult, steps: List[Tree] = Nil, override val line: Int) extends Result(line) {
        def userRepr = (steps.map(_.prettyPrint) :+ final_.userRepr).mkString(" => ")
    }
    case class CompileErrorResult(message: String, override val line: Int) extends Result(line) {
        def userRepr = message
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
            case _ => toSource(value) getOrElse value.toString
        }
    }
    implicit def createObjectResult(value: Any) = ObjectResult(value)
    implicit def createBlockFromSingle(single: Result) = BlockResult(List(single))

    val notImplSymbol = Ident(newTermName("$qmark$qmark$qmark"))

    def evaluate(AST: Tree, toolBox: ToolBox[reflect.runtime.universe.type], symbols: List[DefTree] = Nil, enableSteps: Boolean): List[Result] = {
      lazy val classDefs: Traversable[ClassDef] = symbols.collect{ case elem: ClassDef => elem }

      def evaluateWithSymbols(expr: Tree, extraSymbols: Traversable[Tree] = Set()) = {
          // In Scala 2.10.1, when you create a Block using the following deprecated method, if you pass in only one argument
          // it will be a block that adds a unit expressions at it's end and evaluates to unit. Useless behavior as far as we are concerned.
          // TODO: Remove use of deprecated Block constructor.
          val totalSymbols = symbols ++ extraSymbols
          if (totalSymbols.isEmpty) toolBox.eval(expr)
          else toolBox.eval(Block(totalSymbols.toList :+ expr: _*))
      }

      def evaluateTypeDefBody(body: List[Tree], classParams: List[ValDef] = Nil): BlockResult = {
            val result = body.map { child =>
                // One (such as myself) might think it would be a good idea to remove the current
                // member from the body (list of members) and only supply all other members
                // but it turns out that because of potential circular dependencies and recursive function
                // definitions that it is better to just supply them all including the member we are currently
                // evaluating.
                // We only grab the rhs of the current member when evaluating so, it's not like
                // the compiler is going to complain about a duplicate definition.
                val newSymbols = updateSymbols(symbols, (classParams :+ AST) ::: body)
                evaluate(child, toolBox, newSymbols, enableSteps)
            }
            BlockResult(result.flatten)
      }

      def updateSymbols(oldSymbols: List[DefTree], newSymbols: List[Tree]): List[DefTree] = {
        val newDefSymbols = newSymbols.collect{ case s: DefTree => s }
        val oldInNewScope = oldSymbols.filter( symbol => !newDefSymbols.exists(symbol.name.toString == _.name.toString))
        oldInNewScope ++ newDefSymbols
      }

      object StepTransformer extends Transformer {
        def firstStep(tree: Tree): Tree = tree match {
          case _: Ident => tree
          case _ => transform(tree)
        }
        override def transform(tree: Tree): Tree = tree match {
          case ident: Ident => {
            val result = evaluateWithSymbols(ident)
            val sourceString = toSource(result)
            sourceString.map(toolBox.parse(_)) getOrElse ident
          }
          case _ => super.transform(tree)
        }
      }


      AST match {
        case ValDef(_, name, _, rhs) => {
            val rhsResult = evaluate(rhs, toolBox, symbols, enableSteps)
            List(ValDefResult(name.toString, None, BlockResult(rhsResult), line = AST.pos.line))
        }
        // We fold over each child and all of it's preceding childs (inits) and evaluate
        // it with it's preceding children
        case expr: Block => expr.children.inits.toList.reverse.drop(1).map { childs =>
            val newSymbols = updateSymbols(symbols, childs.init)
            evaluate(childs.last, toolBox, newSymbols, enableSteps)
        }.flatten
        case classDef: ClassDef =>
            // TODO: Handle abstract classes in a more awesome way than ignoring them
            if (classDef.isAbstract) Nil
            else
                classDef.constructorOption.flatMap { constructor =>
                    constructor.sampleParamsOption(classDefs).map { sampleValues =>
                        // We remove the value defintions resulting from the class parameters and the primary constructor
                        val body = classDef.impl.body.filter {
                            case valDef: ValDef => !sampleValues.exists( sampleValDef => sampleValDef.name == valDef.name)
                            case other => other != constructor
                        }
                        val bodyResult = evaluateTypeDefBody(body, sampleValues)
                        List(ClassDefResult(classDef.name.toString,paramList(sampleValues), bodyResult, line = AST.pos.line))
                    }
                } getOrElse {
                    Nil
                }
        case moduleDef: ModuleDef => {
            val body = moduleDef.impl.body.filter(!isConstructor(_))
            List(ModuleDefResult(moduleDef.name.toString, evaluateTypeDefBody(body), line = AST.pos.line))
        }
        case defdef : DefDef => {
            defdef.sampleParamsOption(classDefs) map { sampleValues =>
                val newSymbols = updateSymbols(symbols, sampleValues)
                val rhs = evaluate(defdef.rhs, toolBox, newSymbols, enableSteps)
                List(DefDefResult(defdef.name.toString, paramList(sampleValues), None, line = AST.pos.line, rhs = BlockResult(rhs)))
            } getOrElse {
                Nil
            }
        }
        case EmptyTree => Nil
        case expr => {
            val result: ValueResult = if (expr.equalsStructure(notImplSymbol)) NotImplementedResult else evaluateWithSymbols(AST)

            val steps =
				if (enableSteps) {
					val firstStep = StepTransformer.firstStep(expr)
					if(firstStep.equalsStructure(expr)) Nil else List(firstStep)
				}
				else Nil
            List(ExpressionResult(final_ = result , steps = steps, line = AST.pos.line))
        }
      }
    }

    def computeResults(code: String, enableSteps: Boolean = true): BlockResult = try {
        val toolBox = cm.mkToolBox()
        val AST = toolBox.parse(code)
        BlockResult(evaluate(AST, toolBox, enableSteps = enableSteps))
    } catch {
      case ToolBoxError(msg, cause) => {
        val userMessage = msg.dropWhile(_ != ':').drop(2).dropWhile(char => char == ' ' || char == '\n' || char == '\t')
        BlockResult(CompileErrorResult(userMessage, 0) :: (if (code.lines.isEmpty) Nil else computeResults(code.lines.drop(1).mkString).children))
      }
    }

    implicit class DefDefWithSamples(defdef: DefDef) {
        def sampleParamsOption(classDefs: Traversable[ClassDef] = Nil,
                               samplePool: SamplePool = createDefaultSamplePool): Option[List[ValDef]] = {
            def sequentialEagerTerminationImpl(valDefs: List[ValDef]): Option[List[ValDef]] = {
                if (valDefs.isEmpty) Some(Nil)
                else {
                    valDefs.head.withSampleValue(classDefs, samplePool).map { newValDef =>
                        sequentialEagerTerminationImpl(valDefs.tail).map { rest =>
                            newValDef :: rest
                        }
                    }.flatten
                }
            }
            // TODO: Handle currying better
            val allParams = defdef.vparamss.flatten
            sequentialEagerTerminationImpl(allParams)
        }
    }

    implicit class ValDefWithSample(valDef: ValDef) {

        def withSampleValue(classDefs: Traversable[ClassDef] = Nil,
                            samplePool: SamplePool = createDefaultSamplePool): Option[ValDef] =
            if (valDef.rhs.isEmpty)
                valDef.sampleValue(classDefs, samplePool).map { rhs =>
                    ValDef(Modifiers(), valDef.name, TypeTree(), rhs)
                }
            else
                Some(valDef)

        def sampleValue(classDefs: Traversable[ClassDef] = Nil,
                        samplePool: SamplePool = createDefaultSamplePool): Option[Tree] = {
            valDef.tpt.sampleValue(classDefs, samplePool)
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
                        val implementedMembersOption: List[Option[ValOrDefDef]] = classDef.abstractMembers.map {
                            case valDef: ValDef => valDef.withSampleValue(classDefs, samplePool)
                            case defDef: DefDef => defDef.tpt.sampleValue(classDefs, samplePool).map { sampleImpl =>
                                DefDef(Modifiers(), defDef.name, List(), defDef.vparamss, TypeTree(), sampleImpl)
                            }
                        }
                        if (implementedMembersOption.exists(_.isEmpty)) None
                        else {
                            val implementedMembers = implementedMembersOption.map(_.get)
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