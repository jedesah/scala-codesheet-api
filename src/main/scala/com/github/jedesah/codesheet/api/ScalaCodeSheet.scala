package com.github.jedesah.codesheet.api

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

import com.github.jedesah.Math

object ScalaCodeSheet {

    def evaluate(AST: Tree, outputResult: List[String], toolBox: ToolBox[reflect.runtime.universe.type], symbols: Set[Tree] = Set()): List[String] = {
      lazy val classDefs = symbols.filter(_.isInstanceOf[ClassDef]).map(_.asInstanceOf[ClassDef])
      def updateOutput(newOutput: String) = {
          // -1 because the in memory compiler wraps the code in curly braces to form a block
          // -1 because we are dealing with a zero based collection but 1 based lines in the string
          val index = AST.pos.line - 2
          val previousOutputOnLine = outputResult(index)
          // We could conceivably have more than one result on a single line
          val updatedOutput = if (previousOutputOnLine == "") newOutput else previousOutputOnLine + " ; " + newOutput
          outputResult.updated(index, updatedOutput)
      }
      def evaluateWithSymbols(expr: Tree, extraSymbols: Traversable[Tree] = Set()) = {
          // In Scala 2.10.1, when you create a Block using the following deprecated method, if you pass in only one argument
          // it will be a block that adds a unit expressions at it's end and evaluates to unit. Useless behavior as far as we are concerned.
          // TODO: Remove use of deprecated Block constructor.
          val totalSymbols = symbols ++ extraSymbols
          if (totalSymbols.isEmpty) toolBox.eval(expr)
          else toolBox.eval(Block(totalSymbols.toList :+ expr: _*))
      }
      AST match {
        // There is what appears to be a bug in Scala 2.10.1 where you cannot use a capital letter in a case statement with a @
        case ast @ ValDef(_, newTermName, _, assignee) => {
          val evaluatedAssignement = evaluateWithSymbols(assignee)
          val output = s"$newTermName = $evaluatedAssignement"
          updateOutput(output)
        }
        case expr: Block => expr.children.foldLeft(outputResult) { (result, child) => evaluate(child, result, toolBox, symbols ++ expr.children)}
        case _ : ClassDef => outputResult
        case defdef : DefDef => {
            val sampleValues = assignSampleValues(defdef.vparamss.flatten, defaultSamplePool, classDefs, toolBox)
            val newOutputResult =
                if (sampleValues.exists(_.rhs.isEmpty)) outputResult
                else {
                    val sampleResult = evaluateWithSymbols(defdef.rhs, sampleValues).toString
                    val paramList:String = sampleValues.map( valDef => valDef.name + " = " + prettyPrint(valDef.rhs)).mkString(", ")
                    val lhs:String = defdef.name + (if (paramList.isEmpty) "" else s"($paramList)")
                    val output = s"$lhs => $sampleResult"
                    updateOutput(output)
                }
            
            newOutputResult
        }
        case _ => {
          val result = evaluateWithSymbols(AST)
          val output = result match {
            case _ : scala.runtime.BoxedUnit => ""
            case _ => result.toString
          }
          updateOutput(output)
        }
      }
    }

    
    def prettyPrint(tree: Tree): String = tree match {
      case tree: Apply => tree.fun match {
          case fun: Select => fun.qualifier.toString + "(" + tree.args.mkString(", ") + ")"
          case fun: Ident => fun.toString + "(" + tree.args.mkString(", ") + ")"
      }
      case _ => tree.toString
    }

    def computeResults(code: String): List[String] = try {
        val toolBox = cm.mkToolBox()
        val AST = toolBox.parse(code)
        val outputResult = (0 until code.lines.size).map( _ => "").toList
        evaluate(AST, outputResult, toolBox)
    } catch {
      case ToolBoxError(msg, cause) => {
        val userMessage = msg.dropWhile(_ != ':').drop(2).dropWhile(char => char == ' ' || char == '\n' || char == '\t')
        userMessage :: (if (code.lines.isEmpty) Nil else computeResults(code.lines.drop(1).mkString))
      }
    }

    /**
      * Given a set of value definitions, will return new non-empty value definitions with sample values.
      * If the a value definition is already non-empty or no sample value could be created, it will be returned unchanged
    */
    def assignSampleValues(original: List[ValDef],
                           samplePool: SamplePool,
                           classDefs: Set[ClassDef],
                           toolBox: ToolBox[reflect.runtime.universe.type]): List[ValDef] = {
        def assignCaseClassSampleValue(type_ : Tree): Option[(String, SamplePool)] =
            classDefs.find(_.name.toString == type_.toString).map { classDef =>
                val isConstructor = (member: Tree) => {
                    member match {
                        case defdef: DefDef => defdef.name.toString == "<init>"
                        case _ => false
                    }
                } 
                val constructor:Option[DefDef] = classDef.impl.body.find(isConstructor).asInstanceOf[Option[DefDef]]
                constructor.map { constructorDef =>
                    val innerSamples = assignSampleValues(constructorDef.vparamss.flatten, samplePool, classDefs, toolBox)
                    // TODO: Actually remove sample values from the samplePool instead of just returning it unchanged
                    // TODO: Don't assume it's a case class
                    val sample:String = classDef.name + "(" + innerSamples.map(_.rhs).mkString(",") + ")"
                    (sample, samplePool)
                }
                
            }.flatten
        if (original.isEmpty) Nil
        else {
            val valDef = original.head
            val change: Option[(String, SamplePool)] =
                if (!valDef.rhs.isEmpty) None
                else {
                    valDef.tpt match {
                        case tpt: AppliedTypeTree => {
                            val innerType = tpt.args(0).toString
                            tpt.tpt.toString match {
                                case "List" => samplePool.getList(innerType)
                                case "Option" => samplePool.getOption(innerType)
                                case _ => None
                            } 
                        }
                        case tpt => samplePool.get(tpt.toString).orElse(assignCaseClassSampleValue(tpt))
                    }
                }
            val (newValDef, newSamplePool) = change.map {
                case (sampleRHS, newSamplePool) =>
                    val newValDef = ValDef(Modifiers(), valDef.name, TypeTree(), toolBox.parse(sampleRHS))
                    (newValDef, newSamplePool)
            } getOrElse {
                (valDef, samplePool)
            }
            newValDef :: assignSampleValues(original.tail, newSamplePool, classDefs, toolBox)
        }
    }

    // I don't like the fact that 2 + 3 = 5 which is why I'd rather
    // start the sample Int values at 3 instead of at 2 in the primeNumbers list

    val sampleIntValues = Math.primeNumbers.drop(1).map(_.toString)
    val sampleStringValues = "\"foo\"" #:: "\"bar\"" #:: "\"biz\"" #:: "\"says\"" #:: "\"Hello\"" #:: "\"World\"" #:: "\"bazinga\"" #:: Stream.empty repeat 
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val sampleCharValues = (0 until alphabet.length).map(alphabet.charAt(_)).toStream.repeat.map("\'" + _ + "\'")
    val sampleFloatValues = Math.primeNumbers.map(_ - 0.5).map(_.toString)
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
      "Double" -> sampleFloatValues,
      "Byte" -> sampleIntValues,
      "Short" -> sampleIntValues,
      "Char" -> sampleCharValues,
      "AnyVal" -> sampleAnyValValues,
      "Any" -> sampleAnyValues,
      "AnyRef" -> sampleAnyRefValues
    )

    val defaultSamplePool = SamplePool(simpleValues, sampleSeqLengths, sampleSomeOrNone)
}

case class SamplePool(values: Map[String, Stream[String]], seqLengths: Stream[Int], someOrNone: Stream[Boolean]) {
    def get(type_ : String): Option[(String, SamplePool)] =
        values.get(type_).map { stream => 
            (stream.head, this.copy(values = values.updated(type_, stream.tail)))
        }
    def getList(type_ : String): Option[(String, SamplePool)] =
        values.get(type_).map { valuesForType =>
            val (innerSamples, rest) = valuesForType.splitAt(seqLengths.head)
            val sample:String = if (seqLengths.head == 0) "Nil" else "List(" + innerSamples.mkString(",") + ")"
            (sample, this.copy( values = values.updated(type_, rest), seqLengths = seqLengths.tail))
        }
    def getOption(type_ : String): Option[(String, SamplePool)] =
        values.get(type_).map { valuesForType =>
            val (sample, newValues) =
              if (someOrNone.head) ("Some(" + valuesForType.head + ")", values.updated(type_, valuesForType.tail))
              else ("None", values)
            (sample, this.copy(values = newValues, someOrNone = someOrNone.tail))
        }
}