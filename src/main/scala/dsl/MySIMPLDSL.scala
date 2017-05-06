package dsl
 
import dsl._
import scala.util.parsing.combinator._
import scala.util.matching._

class MySIMPLDSL {
  
  // Converter
    
  object Test {
    def apply(s: String) = {
      Parser.parseAll(Parser.tester, s)
    }
  }

  // -----------
  // PROGRAM INITIALIZATION
  // -----------
  
  object p extends ProgramEnv(null) {
    
    // RETURN STATEMENT
    override def | (ret: ReturnElement) : ParseEnv = new ProgramEnv(p)|ret
    
    // FUNCTION DECLARATION
    override def | (func: FuncDeclElement) : ProgramEnv = new ProgramEnv(p)|func
    
    // STATEMENTS
    
    // Declaration
    override def | (sym: Symbol) : ProgramEnv = new ProgramEnv(p)|sym
    
    // Assignment
    override def | (asg: AssignmentElement) : ProgramEnv = new ProgramEnv(p)|asg
    
    // DeclAssignment
    override def | (declAsg: DeclAssignmentElement) : ProgramEnv = new ProgramEnv(p)|declAsg
    
    // Conditional
    override def | (cond: ConditionalElement) : ProgramEnv = new ProgramEnv(p)|cond
    
    // Loop
    override def | (loop: LoopElement) : ProgramEnv = new ProgramEnv(p)|loop
    
  }
  
  // ------------
  // ELEMENT SYNTAX
  // ------------
  
  // RUN STATEMENT
  object GO extends RunElement
  
  // RETURN STATEMENT
  object r {
    // Takes a base - variable, number, expression, function call
    def apply(variable: Symbol) : ReturnElement = new ReturnElement(new VariableExpressionElement(variable))
    def apply(number: Double) : ReturnElement = new ReturnElement(new NumberExpressionElement(number))
    def apply(exp: ExpressionElement) : ReturnElement = new ReturnElement(exp)
    def apply(call: FuncCallElement) : ReturnElement = new ReturnElement(call)
  }
  
  // FUNCTION DECLARATION
  implicit class FuncDecl(f: Symbol) {
    def ~ (v: Symbol, env: ParseEnv) : FuncDeclElement = new FuncDeclElement(f, v, env)
  }
  
  // STATEMENTS
  
  // Assignment
  implicit class Assignment(s: Symbol) {
    // Takes a base - variable, number, expression, function call
    def → (variable: Symbol) : AssignmentElement = new AssignmentElement(s, new VariableExpressionElement(variable))
    def → (number: Double) : AssignmentElement = new AssignmentElement(s, new NumberExpressionElement(number))
    def → (exp: ExpressionElement) : AssignmentElement = new AssignmentElement(s, exp)
    def → (call: FuncCallElement) : AssignmentElement = new AssignmentElement(s, call)
  }
  
  // DeclAssignment
  implicit class DeclAssignment(s: Symbol) {
    // Takes a base - variable, number, expression, function call
    def ⟶ (variable: Symbol) : DeclAssignmentElement = new DeclAssignmentElement(s, new VariableExpressionElement(variable))
    def ⟶ (number: Double) : DeclAssignmentElement = new DeclAssignmentElement(s, new NumberExpressionElement(number))
    def ⟶ (exp: ExpressionElement) : DeclAssignmentElement = new DeclAssignmentElement(s, exp)
    def ⟶ (call: FuncCallElement) : DeclAssignmentElement = new DeclAssignmentElement(s, call)
  }
  
  // CONDITIONALS
  
  abstract class Conditional {
    var c : ConditionElement = null
    
    // If-then
    def ? (s: StatementSeqElement) = new ConditionalWithoutElseElement(c, s)
    def ? (s: StatementElement) = new ConditionalWithoutElseElement(c, new SingleStatementSeqElement(s))
    def ? (s: Symbol) = new ConditionalWithoutElseElement(c, new SingleStatementSeqElement(new DeclarationElement(s)))
    
    // If-then-else
    def ? (s1: StatementSeqElement, s2: StatementSeqElement) = new ConditionalWithElseElement(c, s1, s2)
    def ? (s1: StatementSeqElement, s2: StatementElement) = new ConditionalWithElseElement(c, s1, new SingleStatementSeqElement(s2))
    def ? (s1: StatementSeqElement, s2: Symbol) = new ConditionalWithElseElement(c, s1, new SingleStatementSeqElement(new DeclarationElement(s2)))
    
    def ? (s1: StatementElement, s2: StatementSeqElement) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(s1), s2)
    def ? (s1: StatementElement, s2: StatementElement) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(s1), new SingleStatementSeqElement(s2))
    def ? (s1: StatementElement, s2: Symbol) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(s1), new SingleStatementSeqElement(new DeclarationElement(s2)))

    def ? (s1: Symbol, s2: StatementSeqElement) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(new DeclarationElement(s1)), s2)
    def ? (s1: Symbol, s2: StatementElement) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(new DeclarationElement(s1)), new SingleStatementSeqElement(s2))
    def ? (s1: Symbol, s2: Symbol) = new ConditionalWithElseElement(c, new SingleStatementSeqElement(new DeclarationElement(s1)), new SingleStatementSeqElement(new DeclarationElement(s2)))
  }
  
  implicit class MySIMPLConditional(cond: ConditionElement) extends Conditional {
    c = cond
  }
  
  implicit class ScalaConditional(bool: Boolean) extends Conditional {
    if(bool) c = new TrueConditionElement
    else c = new FalseConditionElement
  }
  
  // Loop
  implicit class Loop(c: ConditionElement) {
    def ~ (s: StatementSeqElement) = new LoopElement(c, s)
    def ~ (s: StatementElement) = new LoopElement(c, new SingleStatementSeqElement(s))
    def ~ (s: Symbol) = new LoopElement(c, new SingleStatementSeqElement(new DeclarationElement(s)))
  }
  
  // STATEMENT SEQUENCES
  
  // Pair of single statements
  implicit class PairStatementSeq(s1: StatementElement) {
    def | (s2: StatementElement) = new MultipleStatementSeqElement(new SingleStatementSeqElement(s1), s2)
    def | (s2: Symbol) = new MultipleStatementSeqElement(new SingleStatementSeqElement(s1), new DeclarationElement(s2))
  }
  
  implicit class SymbolPairStatementSeq(s1: Symbol) {
    def | (s2: StatementElement) = new MultipleStatementSeqElement(new SingleStatementSeqElement(new DeclarationElement(s1)), s2)
    def | (s2: Symbol) = new MultipleStatementSeqElement(new SingleStatementSeqElement(new DeclarationElement(s1)), new DeclarationElement(s2))
  }
  
  // Multiple statements
  implicit class MultipleStatementSeq(seq: StatementSeqElement) {
    def | (s: StatementElement) = new MultipleStatementSeqElement(seq, s)
    def | (s: Symbol) = new MultipleStatementSeqElement(seq, new DeclarationElement(s))
  }
  
  // CONDITIONS
  
  // Abstract class for comparison conditions
  // Both parameters can be base elements (variables, numbers, expressions, function calls)
  // So we need to iterate through all possibilities
  abstract class ComparisonCondition {
    var b1 : ExpressionElement = null

    // Variables
    def ≔ (b2: Symbol) = new ComparisonConditionElement(b1, EqualCompElement, new VariableExpressionElement(b2))
    def ≠ (b2: Symbol) = new ComparisonConditionElement(b1, NotEqualCompElement, new VariableExpressionElement(b2))
    def > (b2: Symbol) = new ComparisonConditionElement(b1, GreaterCompElement, new VariableExpressionElement(b2))
    def < (b2: Symbol) = new ComparisonConditionElement(b1, LessCompElement, new VariableExpressionElement(b2))
    def ≥ (b2: Symbol) = new ComparisonConditionElement(b1, GreaterOrEqualCompElement, new VariableExpressionElement(b2))
    def ≤ (b2: Symbol) = new ComparisonConditionElement(b1, LessOrEqualCompElement, new VariableExpressionElement(b2))
    
    // Numbers
    def ≔ (b2: Double) = new ComparisonConditionElement(b1, EqualCompElement, new NumberExpressionElement(b2))
    def ≠ (b2: Double) = new ComparisonConditionElement(b1, NotEqualCompElement, new NumberExpressionElement(b2))
    def > (b2: Double) = new ComparisonConditionElement(b1, GreaterCompElement, new NumberExpressionElement(b2))
    def < (b2: Double) = new ComparisonConditionElement(b1, LessCompElement, new NumberExpressionElement(b2))
    def ≥ (b2: Double) = new ComparisonConditionElement(b1, GreaterOrEqualCompElement, new NumberExpressionElement(b2))
    def ≤ (b2: Double) = new ComparisonConditionElement(b1, LessOrEqualCompElement, new NumberExpressionElement(b2))
    
    // Expressions
    def ≔ (b2: ExpressionElement) = new ComparisonConditionElement(b1, EqualCompElement, b2)
    def ≠ (b2: ExpressionElement) = new ComparisonConditionElement(b1, NotEqualCompElement, b2)
    def > (b2: ExpressionElement) = new ComparisonConditionElement(b1, GreaterCompElement, b2)
    def < (b2: ExpressionElement) = new ComparisonConditionElement(b1, LessCompElement, b2)
    def ≥ (b2: ExpressionElement) = new ComparisonConditionElement(b1, GreaterOrEqualCompElement, b2)
    def ≤ (b2: ExpressionElement) = new ComparisonConditionElement(b1, LessOrEqualCompElement, b2)
    
    // Function calls
    def ≔ (b2: FuncCallElement) = new ComparisonConditionElement(b1, EqualCompElement, b2)
    def ≠ (b2: FuncCallElement) = new ComparisonConditionElement(b1, NotEqualCompElement, b2)
    def > (b2: FuncCallElement) = new ComparisonConditionElement(b1, GreaterCompElement, b2)
    def < (b2: FuncCallElement) = new ComparisonConditionElement(b1, LessCompElement, b2)
    def ≥ (b2: FuncCallElement) = new ComparisonConditionElement(b1, GreaterOrEqualCompElement, b2)
    def ≤ (b2: FuncCallElement) = new ComparisonConditionElement(b1, LessOrEqualCompElement, b2)
  }
  
  implicit class VariableComparisonCondition(variable: Symbol) extends ComparisonCondition {
    b1 = new VariableExpressionElement(variable)
  }
  
  implicit class NumberComparisonCondition(number: Double) extends ComparisonCondition {
    b1 = new NumberExpressionElement(number)
  }
  
  implicit class ExpressionComparisonCondition(exp: ExpressionElement) extends ComparisonCondition {
    b1 = exp
  }
  
  implicit class FuncCallComparisonCondition(funcCall: FuncCallElement) extends ComparisonCondition {
    b1 = funcCall
  }
  
  // Logical Condition
  implicit class LogicalCondition(c1: ConditionElement) {
    def && (c2: ConditionElement) = new LogicalConditionElement(c1, AndOpElement, c2)
    def || (c2: ConditionElement) = new LogicalConditionElement(c1, OrOpElement, c2)
  }
  
  // Boolean Conditions
  object T extends TrueConditionElement
  object F extends FalseConditionElement
  
  // Not Condition
  object ¬ {
    def apply(c: ConditionElement) = new NotConditionElement(c)
  }
  
  // FUNCTION CALL
  
  implicit class FuncCall(functionName: Symbol) {
    // Takes a base - variable, number, expression, function call
    def ~ (variable: Symbol) = new FuncCallElement(functionName, new VariableExpressionElement(variable))
    def ~ (number: Double) = new FuncCallElement(functionName, new NumberExpressionElement(number))
    def ~ (exp: ExpressionElement) = new FuncCallElement(functionName, exp)
    def ~ (funcCall: FuncCallElement) = new FuncCallElement(functionName, funcCall)
  }
  
  // EXPRESSION OPERATIONS
  
  // Expressions are defined recursively as an expression on the LHS and RHS
  abstract class ExpressionOperation {
    var exp1: ExpressionElement = null
    
    // Variables
    def + (variable: Symbol) = new ExpressionSeqElement(exp1, PlusOpElement, new VariableExpressionElement(variable))
    def - (variable: Symbol) = new ExpressionSeqElement(exp1, MinusOpElement, new VariableExpressionElement(variable))
    def * (variable: Symbol) = new ExpressionSeqElement(exp1, MultiplyOpElement, new VariableExpressionElement(variable))
    def / (variable: Symbol) = new ExpressionSeqElement(exp1, DivideOpElement, new VariableExpressionElement(variable))
    
    // Numbers
    def + (number: Double) = new ExpressionSeqElement(exp1, PlusOpElement, new NumberExpressionElement(number))
    def - (number: Double) = new ExpressionSeqElement(exp1, MinusOpElement, new NumberExpressionElement(number))
    def * (number: Double) = new ExpressionSeqElement(exp1, MultiplyOpElement, new NumberExpressionElement(number))
    def / (number: Double) = new ExpressionSeqElement(exp1, DivideOpElement, new NumberExpressionElement(number))
    
    // Expressions
    def + (exp2: ExpressionElement) = new ExpressionSeqElement(exp1, PlusOpElement, exp2)
    def - (exp2: ExpressionElement) = new ExpressionSeqElement(exp1, MinusOpElement, exp2)
    def * (exp2: ExpressionElement) = new ExpressionSeqElement(exp1, MultiplyOpElement, exp2)
    def / (exp2: ExpressionElement) = new ExpressionSeqElement(exp1, DivideOpElement, exp2)
    
    // Function Calls
    def + (funcCall: FuncCallElement) = new ExpressionSeqElement(exp1, PlusOpElement, funcCall)
    def - (funcCall: FuncCallElement) = new ExpressionSeqElement(exp1, MinusOpElement, funcCall)
    def * (funcCall: FuncCallElement) = new ExpressionSeqElement(exp1, MultiplyOpElement, funcCall)
    def / (funcCall: FuncCallElement) = new ExpressionSeqElement(exp1, DivideOpElement, funcCall)
  }
  
  implicit class VariableExpressionOperation(variable: Symbol) extends ExpressionOperation {
    exp1 = new VariableExpressionElement(variable)
  }
  
  implicit class NumberExpressionOperation(number: Double) extends ExpressionOperation {
    exp1 = new NumberExpressionElement(number)
  }
  
  implicit class RecursiveExpressionOperation(expression: ExpressionElement) extends ExpressionOperation {
    exp1 = expression
  }
  
  implicit class FuncCallExpressionOperation(funcCall: FuncCallElement) extends ExpressionOperation {
    exp1 = funcCall
  }
  
}