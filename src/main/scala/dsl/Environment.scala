package dsl

import scala.collection.mutable.HashMap
import scala.List

object ProgramException extends Exception

class ScopeEnv {
  var vars = HashMap.empty[Symbol, ExpressionElement]
  var funcs = HashMap.empty[Symbol, FuncDeclElement]
}

class EnvWrapper {
  var env = new RunTimeEnv
}

class RunTimeEnv {
  var scopes : List[ScopeEnv] = List.empty[ScopeEnv]
  scopes = scopes :+ new ScopeEnv

  // Convenience functions
  def varsAtIndex(i: Int) : HashMap[Symbol, ExpressionElement] = scopes(i).vars
  def funcsAtIndex(i: Int) : HashMap[Symbol, FuncDeclElement] = scopes(i).funcs
}

class ParseEnv(l: List[StatementElement]) {
  
  // INITIALIZATION
  
  var stop : Boolean = false
  var lines = l
  if(lines == null) {
    lines = List.empty[StatementElement]
  }
  
  private def exitWithError(message: String) {
    println("ERROR: " + message)
    stop = true 
  }
  
  // RUN PROGRAM
  
  def | (run: RunElement) : Double = {
    val env : EnvWrapper = new EnvWrapper
    return runProgram(env, lines)
  }
  
  private def runProgram(env: EnvWrapper, lines: List[StatementElement]) : Double = {
    //println("Running " + lines.length + " lines...")
    var result : Double = -1
    try {
      for(line <- lines) {
        //println(line.printMessage)
        val tempResult = evaluate(env, line)
        if(stop) {
          throw ProgramException
        }
        if(tempResult.isInstanceOf[Double]) {
          result = tempResult.asInstanceOf[Double]
        }
      }
    } catch {
      case ProgramException => println("Program terminating with result -1.")
    }
    return result
  }
  
  // EVALUATE STATEMENTS
  
  private def evaluate (env: EnvWrapper, stmt: StatementElement) : Any = {
    stmt match {
      case ret: ReturnElement => evaluateStatement(env, ret)
      case funcDecl: FuncDeclElement => evaluateStatement(env, funcDecl)
      case decl: DeclarationElement => evaluateStatement(env, decl)
      case asgn: AssignmentElement => evaluateStatement(env, asgn)
      case declAsgn: DeclAssignmentElement => evaluateStatement(env, declAsgn)
      case cond: ConditionalElement => evaluateStatement(env, cond)
      case loop: LoopElement => evaluateStatement(env, loop)
      case _ => exitWithError("Unrecognized program statement.")
    }
  }
  
  private def evaluateStatement (env: EnvWrapper, ret: ReturnElement) : Double = {
    return evaluateExpression(env, ret.expression)
  }
  
  private def evaluateStatement (env: EnvWrapper, funcDecl: FuncDeclElement) {
    // Check that the function has not already been defined in the current scope. If so, define it.
    val currentFuncs = env.env.funcsAtIndex(0)
    if(currentFuncs.keySet.contains(funcDecl.name)) {
      exitWithError("Cannot re-define a function that is already defined in the current scope.")
    }
    else {
      currentFuncs.put(funcDecl.name, funcDecl)
    }
  }
  
  private def evaluateStatement (env: EnvWrapper, decl: DeclarationElement) {
    // Check that the variable is not already declared in the current scope. If so, declare it.
    val currentVars = env.env.varsAtIndex(0)
    if(currentVars.keySet.contains(decl.name)) {
      exitWithError("Cannot re-declare a variable that is already declared in the current scope.")
    }
    else {
      currentVars.put(decl.name, null)
    }
  }
  
  private def evaluateStatement (env: EnvWrapper, asgn: AssignmentElement) {
    // Check that the variable is already declared in any scope. If so, assign it the given value
    for(i <- 0 to env.env.scopes.length - 1) {
      val currentVars = env.env.varsAtIndex(i)
      if(currentVars.keySet.contains(asgn.name)) {
        val result = evaluateExpression(env, asgn.expression)
        val exp = new NumberExpressionElement(result)
        currentVars.update(asgn.name, exp)
        return
      }
    }
    exitWithError("Cannot assign a variable that has not been declared in any scope: " + asgn.name + ".")
  }
  
  private def evaluateStatement (env: EnvWrapper, declAsgn: DeclAssignmentElement) {
    // Check that the variable is not yet declared in the current scope. If so, declare it and assign the given value.
    val currentVars = env.env.varsAtIndex(0)
    if(currentVars.keySet.contains(declAsgn.name)) {
      exitWithError("Cannot re-declare a variable that is already declared in the current scope.")
    }
    else {
      val result = evaluateExpression(env, declAsgn.expression)
      val exp = new NumberExpressionElement(result)
      currentVars.put(declAsgn.name, exp)
    }
  }
  
  private def evaluateStatement (env: EnvWrapper, conditional: ConditionalElement) {
    conditional match {
      case withoutElse: ConditionalWithoutElseElement => evaluateStatement(env, withoutElse)
      case withElse: ConditionalWithElseElement => evaluateStatement(env, withElse)
      case _ => exitWithError("Unrecognized conditional type.")
    }
  }
  
  private def evaluateStatement (env: EnvWrapper, conditional: ConditionalWithoutElseElement) {
    // Evaluate the condition, then the statement sequence if true
    if(evaluateCondition(env, conditional.cond)) evaluateStatementSeq(env, conditional.trueSeq)
  }
  
  private def evaluateStatement (env: EnvWrapper, conditional: ConditionalWithElseElement) {
    // Evaluate the condition, then the corresponding statement sequence
    if(evaluateCondition(env, conditional.cond)) evaluateStatementSeq(env, conditional.trueSeq)
    else evaluateStatementSeq(env, conditional.falseSeq)
  }
  
  private def evaluateStatement (env: EnvWrapper, loop: LoopElement) {
    while(evaluateCondition(env, loop.cond)) {
      evaluateStatementSeq(env, loop.seq)
    }
  }
  
  // EVALUATE STATEMENT SEQUENCES
  
  private def evaluateStatementSeq (env: EnvWrapper, seq: StatementSeqElement) {
    for(statement <- seq.statements) {
      evaluate(env, statement)
    }
  }
  
  // EVALUATE CONDITIONS
  
  private def evaluateCondition (env: EnvWrapper, cond: ConditionElement) : Boolean = {
    cond match {
      case comp: ComparisonConditionElement => evaluateCondition(env, comp)
      case log: LogicalConditionElement => evaluateCondition(env, log)
      case bool: BooleanConditionElement => evaluateCondition(env, bool)
      case not: NotConditionElement => evaluateCondition(env, not)
      case _ => exitWithError("Unrecognized condition type."); return false
    }
  }
  
  private def evaluateCondition (env: EnvWrapper, comp: ComparisonConditionElement) : Boolean = {
    val e1 : Double = evaluateExpression(env, comp.exp1)
    val e2 : Double = evaluateExpression(env, comp.exp2)
    
    comp.comparator match {
      case EqualCompElement => return (e1 == e2)
      case NotEqualCompElement => return (e1 != e2)
      case GreaterCompElement => return (e1 > e2)
      case LessCompElement => return (e1 < e2)
      case LessOrEqualCompElement => return (e1 <= e2)
      case GreaterOrEqualCompElement => return (e1 >= e2)
      case _ => exitWithError("Unrecognized condition comparator."); return false
    }
  }
  
  private def evaluateCondition (env: EnvWrapper, log: LogicalConditionElement) : Boolean = {
    val c1 : Boolean = evaluateCondition(env, log.cond1)
    val c2 : Boolean = evaluateCondition(env, log.cond2)
    
    log.operator match {
      case AndOpElement => return (c1 && c2)
      case OrOpElement => return (c1 || c2)
      case _ => exitWithError("Unrecognized logical operator."); return false
    }
  }
  
  private def evaluateCondition (env: EnvWrapper, bool: BooleanConditionElement) : Boolean = {
    bool match {
      case t: TrueConditionElement => return true
      case f: FalseConditionElement => return false
      case _ => exitWithError("Unrecognized boolean condition type."); return false
    }
  }
  
  private def evaluateCondition (env: EnvWrapper, not: NotConditionElement) : Boolean = {
    val cond : Boolean = evaluateCondition(env, not.condition)
    return !cond
  }

  // EVALUATE EXPRESSIONS
  
  def evaluateExpression (env: EnvWrapper, exp: ExpressionElement) : Double = {
    exp match {
      case variable: VariableExpressionElement => evaluateExpression(env, variable)
      case number: NumberExpressionElement => evaluateExpression(number)
      case exp: ExpressionSeqElement => evaluateExpression(env, exp)
      case funcCall: FuncCallElement => evaluateExpression(env, funcCall)
      case _ => exitWithError("Unrecognized expression type."); return -1
    }
  }
  
  def evaluateExpression (env: EnvWrapper, variable: VariableExpressionElement) : Double = {
    // Look for the variable starting from the nearest scope. If found, evaluate the expression it is mapped to.
    for(i <- 0 to env.env.scopes.length - 1) {
      val currentVars = env.env.varsAtIndex(i)
      if(currentVars.keySet.contains(variable.name)) {
        val value : ExpressionElement = currentVars.getOrElse(variable.name, null)
        if(value == null) {
          exitWithError("Trying to access declared but undefined variable " + variable.name + ".")
        }
        else {
          return evaluateExpression(env, value)
        }
      }
    }
    exitWithError("Variable " + variable.name + " cannot be evaluated because it was not found in any scope.")
    return -1
  }
  
  def evaluateExpression (number: NumberExpressionElement) : Double = {
    // Return the number
    return number.value
  }
  
  def evaluateExpression (env: EnvWrapper, exp: ExpressionSeqElement) : Double = {
    // Recursively evaluate the expression
    //println(exp.printMessage)
    val e1 : Double = evaluateExpression(env, exp.expression1)
    val e2 : Double = evaluateExpression(env, exp.expression2)
    
    exp.operator match {
      case PlusOpElement => (e1 + e2)
      case MinusOpElement => (e1 - e2)
      case MultiplyOpElement => (e1 * e2)
      case DivideOpElement => (e1 / e2)
      case _ => exitWithError("Unrecognized expression operator"); return -1
    }
  }
  
  def evaluateExpression (env: EnvWrapper, funcCall: FuncCallElement) : Double = {
    // Look for the function definition starting from the nearest scope. If found, evaluate the statements it is mapped to.
    for(i <- 0 to env.env.scopes.length - 1) {
      val currentFuncs = env.env.funcsAtIndex(i)
      if(currentFuncs.keySet.contains(funcCall.name)) {
        val funcDeclElement : FuncDeclElement = currentFuncs.getOrElse(funcCall.name, null)
        if(funcDeclElement == null) {
          exitWithError("Trying to access declared but undefined function " + funcCall.name + ".")
        }
        else {
          // Create a new scope and add the function variable to it. Then evaluate the function declaration.
          val funcDeclVariableValue : Double = evaluateExpression(env, funcCall.variable)
          val newEnv = new EnvWrapper
          newEnv.env.scopes = new ScopeEnv +: List[ScopeEnv](env.env.scopes.last)
          newEnv.env.varsAtIndex(0).put(funcDeclElement.variable, new NumberExpressionElement(funcDeclVariableValue))
          val result : Double = runProgram(newEnv, funcDeclElement.program.lines)
          // Remove the function scope from the environment and update the global scope
          newEnv.env.scopes = newEnv.env.scopes.drop(1)
          env.env.scopes = newEnv.env.scopes
          return result
        }
      }
    }
    exitWithError("Function " + funcCall.variable + " not found.")
    return -1
  }
}

class ProgramEnv(env: ProgramEnv) {
  
  // INITIALIZATION
  var lines = List.empty[StatementElement]
  if(env != null) {
    var lines = env.lines
  }
  
  // RETURN STATEMENT
  def | (ret: ReturnElement) : ParseEnv = {
    lines = lines :+ ret
    return new ParseEnv(lines)
  }
  
  // FUNCTION DECLARATION
  def | (func: FuncDeclElement) : ProgramEnv = {
    lines = lines :+ func
    return this
  }
  
  // STATEMENTS
  
  // Declaration
  def | (decl: Symbol) : ProgramEnv = {
    lines = lines :+ new DeclarationElement(decl)
    return this
  }
  
  // Assignment
  def | (asg: AssignmentElement) : ProgramEnv = {
    lines = lines :+ asg
    return this
  }
  
  // DeclAssignment
  def | (declAsg: DeclAssignmentElement) : ProgramEnv = {
    lines = lines :+ declAsg
    return this
  }
  
  // Conditional
  def | (cond: ConditionalElement) : ProgramEnv = {
    lines = lines :+ cond
    return this
  }
  
  // Loop
  def | (loop: LoopElement) : ProgramEnv = {
    lines = lines :+ loop
    return this
  }
}