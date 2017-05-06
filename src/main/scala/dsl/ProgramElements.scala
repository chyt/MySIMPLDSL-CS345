package dsl

import scala.List

abstract class Element {
  def printMessage : String = ""
}

abstract class RunElement

abstract class StatementElement extends Element

// Statements

class ReturnElement(exp: ExpressionElement) extends StatementElement {
  val expression = exp
  
  override def printMessage = "Returning: " + exp.printMessage
}

class FuncDeclElement(n: Symbol, v: Symbol, p: ParseEnv) extends StatementElement {
  val name = n
  val variable = v
  val program = p
  
  override def printMessage = {
    var s = "Function " + name + " - number of lines in function: " + program.lines.length
    for(line <- program.lines) {
      s = s + "\n    " + line.printMessage
    }
    s
  }
}

class DeclarationElement(v: Symbol) extends StatementElement {
  val name = v
  
  override def printMessage = "Declaration: " + name
}

class AssignmentElement(n: Symbol, exp: ExpressionElement) extends StatementElement {
  val name = n
  val expression = exp
  
  override def printMessage = "Assignment: " + name + " to " + expression.printMessage
}

class DeclAssignmentElement(n: Symbol, exp: ExpressionElement) extends StatementElement {
  val name = n
  val expression = exp
  
  override def printMessage = "DeclAssignment: " + name + " to " + expression.printMessage
}

abstract class ConditionalElement extends StatementElement

class ConditionalWithoutElseElement(c: ConditionElement, s: StatementSeqElement) extends ConditionalElement {
  val cond = c
  val trueSeq = s
  
  override def printMessage = "Conditional on: " + cond.printMessage + " | If success: " + trueSeq.printMessage
}

class ConditionalWithElseElement(c: ConditionElement, s1: StatementSeqElement, s2: StatementSeqElement) extends ConditionalElement {
  val cond = c
  val trueSeq = s1
  val falseSeq = s2
  
  override def printMessage = "Conditional on: " + cond.printMessage + " | If success: " + trueSeq.printMessage + " | else: " + falseSeq.printMessage
}

class LoopElement(c: ConditionElement, s: StatementSeqElement) extends StatementElement {
  val cond = c
  val seq = s
  
  override def printMessage = "While: " + cond.printMessage + " | do: " + seq.printMessage
}

// Statement Sequence

abstract class StatementSeqElement extends Element {
  var statements = List.empty[StatementElement]
  
  override def printMessage = {
    var message = "Statement sequence of " + statements.length + " elements: "
    for(statement <- statements) {
      message = message + " " + statement.printMessage + " | "
    }
    message
  }
}

class SingleStatementSeqElement(s: StatementElement) extends StatementSeqElement {
  statements = statements :+ s
}

class MultipleStatementSeqElement(seq: StatementSeqElement, s: StatementElement) extends StatementSeqElement {
  statements = seq.statements :+ s
}

// Condition

abstract class ConditionElement extends Element {
  override def printMessage = {
    this match {
      case comp: ComparisonConditionElement => comp.printMessage
      case log: LogicalConditionElement => log.printMessage
      case bool: BooleanConditionElement => bool.printMessage
      case not: NotConditionElement => not.printMessage
    }
  }
}

class ComparisonConditionElement(e1: ExpressionElement, c: CompElement, e2: ExpressionElement) extends ConditionElement {
  val exp1 = e1
  val comparator = c
  val exp2 = e2
  
  override def printMessage = "Comparison condition: " + exp1.printMessage + " " + comparator.printMessage + " " + exp2.printMessage
}

class LogicalConditionElement(c1: ConditionElement, op: LogOpElement, c2: ConditionElement) extends ConditionElement {
  val cond1 = c1
  val operator = op
  val cond2 = c2
  
  override def printMessage = "Logical condition: " + cond1.printMessage + " " + operator.printMessage + " " + cond2.printMessage
}

abstract class BooleanConditionElement extends ConditionElement {
  override def printMessage = {
    this match {
      case t: TrueConditionElement => t.printMessage
      case f: FalseConditionElement => f.printMessage
    }
  }
}

class TrueConditionElement extends BooleanConditionElement {
  override def printMessage = "TRUE"
}

class FalseConditionElement extends BooleanConditionElement { 
  override def printMessage = "FALSE"
}

class NotConditionElement(c: ConditionElement) extends ConditionElement {
  val condition = c
  
  override def printMessage = "Not condition: " + condition.printMessage
}

// Expressions

abstract class ExpressionElement extends Element {
  override def printMessage = {
    this match {
      case v: VariableExpressionElement => v.printMessage
      case n: NumberExpressionElement => n.printMessage
      case e: ExpressionSeqElement => e.printMessage
      case f: FuncCallElement => f.printMessage
    }
  }
}

class VariableExpressionElement(v: Symbol) extends ExpressionElement {
  val name = v
  
  override def printMessage = "Variable element: " + name
}

class NumberExpressionElement(v: Double) extends ExpressionElement {
  val value = v
  
  override def printMessage = "Number element: " + value
}

// Expression

class ExpressionSeqElement(exp1: ExpressionElement, op: MathOpElement, exp2: ExpressionElement) extends ExpressionElement {
  val expression1 = exp1
  val operator = op
  val expression2 = exp2
  
  override def printMessage = "(Expression sequence: " + expression1.printMessage + " " + operator.printMessage + " " + expression2.printMessage + ")"
}

// Function Call

class FuncCallElement(f: Symbol, b: ExpressionElement) extends ExpressionElement {
  val name = f
  val variable = b
  
  override def printMessage = "Calling function " + name + " with parameter " + variable.printMessage
}

// Addition Operators

abstract class MathOpElement extends Element

abstract class AddOpElement extends MathOpElement

object PlusOpElement extends AddOpElement {
  override def printMessage = "+"
}

object MinusOpElement extends AddOpElement {
  override def printMessage = "-"
}

abstract class MulOpElement extends MathOpElement

object MultiplyOpElement extends MulOpElement {
  override def printMessage = "*"
}

object DivideOpElement extends MulOpElement {
  override def printMessage = "/"
}

// Logical Operators

abstract class LogOpElement extends Element

object AndOpElement extends LogOpElement {
  override def printMessage = "&&"
}

object OrOpElement extends LogOpElement {
  override def printMessage = "||"
}

// Comparison Operators

abstract class CompElement extends Element

object EqualCompElement extends CompElement {
  override def printMessage = "=="
}

object NotEqualCompElement extends CompElement {
  override def printMessage = "!="
}

object LessCompElement extends CompElement {
  override def printMessage = "<"
}

object GreaterCompElement extends CompElement {
  override def printMessage = ">"
}

object LessOrEqualCompElement extends CompElement {
  override def printMessage = "<="
}

object GreaterOrEqualCompElement extends CompElement {
  override def printMessage = ">="
}
