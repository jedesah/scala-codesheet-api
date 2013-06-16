package com.github.jedesah.codesheet.api

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

import com.github.jedesah.Math

object ScalaCodeSheet {

    def evaluate(AST: Tree, outputResult: List[String], toolBox: ToolBox[reflect.runtime.universe.type], symbols: Set[Tree] = Set()): (List[String], Set[Tree]) = {
      def updateOutput(newOutput: String) = {
          // -1 because the in memory compiler wraps the code in curly braces to form a block
          // -1 because we are dealing with a zero based collection but 1 based lines in the string
          val index = AST.pos.line - 2
          val previousOutputOnLine = outputResult(index)
          // We could conceivably have more than one result on a single line
          val updatedOutput = if (previousOutputOnLine == "") newOutput else previousOutputOnLine + " ; " + newOutput
          outputResult.updated(index, updatedOutput)
      }
      def evaluateWithSymbols(expr: Tree, extraSymbols: Set[Tree] = Set()) = {
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
          (updateOutput(output), symbols + AST)
        }
        case expr: Block => expr.children.foldLeft((outputResult, symbols)) { (result, child) => evaluate(child, result._1, toolBox, result._2)}
        case _ : ClassDef => (outputResult, symbols + AST)
        case defdef : DefDef => {
            val sampleValuesPool = createSampleValuePool
            val sampleValues = defdef.vparamss.flatten.map { valDef =>
              ValDef(Modifiers(), valDef.name, TypeTree(), sampleValuesPool(valDef.tpt.toString).next)
            }
            val sampleResult = evaluateWithSymbols(defdef.rhs, sampleValues.toSet).toString
            val paramList = sampleValues.map( valDef => valDef.name + " = " + prettyPrint(valDef.rhs)).mkString(", ")
            val lhs = defdef.name + (if (paramList.isEmpty) "" else s"($paramList)")
            val output = s"$lhs => $sampleResult"
            (updateOutput(output), symbols + AST)
        }
        case _ => {
          val result = evaluateWithSymbols(AST)
          val output = result match {
            case _ : scala.runtime.BoxedUnit => ""
            case _ => result.toString
          }
          (updateOutput(output), symbols)
        }
      }
    }

    def prettyPrint(tree: Tree): String = tree match {
      case tree: Apply => tree.fun.asInstanceOf[Select].qualifier.toString + "(" + tree.args.mkString(", ") + ")"
      case _ => tree.toString
    }

    def computeResults(code: String): List[String] = try {
        val toolBox = cm.mkToolBox()
        val AST = toolBox.parse(code)
        val outputResult = (0 until code.lines.size).map( _ => "").toList
        evaluate(AST, outputResult, toolBox)._1
    } catch {
      case ToolBoxError(msg, cause) => {
        val userMessage = msg.dropWhile(_ != ':').drop(2).dropWhile(char => char == ' ' || char == '\n' || char == '\t')
        userMessage :: (if (code.lines.isEmpty) Nil else computeResults(code.lines.drop(1).mkString))
      }
    }

    // I don't like the fact that 2 + 3 = 5 which is why I'd rather
    // start the sample Int values at 3 instead of at 2 in the primeNumbers list
    val sampleIntValues = Math.primeNumbers.drop(1)
    val sampleStringValues = "foo" #:: "bar" #:: "biz" #:: "says" #:: "Hello" #:: "World" #:: "bazinga" #:: Stream.empty repeat 
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val sampleCharValues = (0 until alphabet.length).map(alphabet.charAt(_)).toStream.repeat 
    val sampleFloatValues = sampleIntValues.map(_ - 0.5)
    val sampleBooleanValues = true #:: false #:: Stream.empty repeat
    //val sampleAnyValValues = sampleIntValues.zip5(sampleStringValues, sampleFloatValues, sampleBooleanValues, sampleCharValues).flatten(_.productIterator)
    val sampleAnyValValues = 3 #:: 'f' #:: true #:: Stream.empty[AnyVal] repeat
    val sampleAnyValues = 3 #:: "foo" #:: true #:: Stream.empty[Any] repeat
    val sampleAnyRefValues = (reify{"foo"} #:: reify{List(3,5,7)} #:: reify{Some(5)} #:: Stream.empty[Expr[AnyRef]]).repeat.map (_.tree)
    val sampleValues = Map(
      "Int" -> (sampleIntValues map ( value => Literal(Constant(value)))),
      "String" -> (sampleStringValues map (value => Literal(Constant(value)))),
      "Float" -> (sampleFloatValues map ( value => Literal(Constant(value)))),
      "Boolean" -> (sampleBooleanValues map ( value => Literal(Constant(value)))),
      "Long" -> (sampleIntValues map ( value => Literal(Constant(value)))),
      "Double" -> (sampleFloatValues map ( value => Literal(Constant(value)))),
      "Byte" -> (sampleIntValues map ( value => Literal(Constant(value)))),
      "Short" -> (sampleIntValues map ( value => Literal(Constant(value)))),
      "Char" -> (sampleCharValues map ( value => Literal(Constant(value)))),
      "AnyVal" -> (sampleAnyValValues map ( value => Literal(Constant(value)))),
      "Any" -> (sampleAnyValues map ( value => Literal(Constant(value)))),
      "AnyRef" -> sampleAnyRefValues
    )
    // Do not simply to use mapValues here because it fucks with how the iterator works here. This is probably a bug.
    def createSampleValuePool = sampleValues.map{ case (key, value) => (key, value.toIterator)}
}