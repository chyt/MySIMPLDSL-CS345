package dsl

import scala.util.parsing.combinator._
import scala.util.matching._

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

trait ConverterObject extends MySIMPLDSL {
  def result : Double = 0.0
}

object Parser extends RegexParsers {
  
  val toolbox = currentMirror.mkToolBox()
  
  // Test case evaluator
  val tester = regexMatch("""[\[](.*)[\]] \| (.*)""".r) ^^ {
    case m => {
      val convertedCode = convert(m.group(1))
      println("---- Begin Conversion ----")
      println("    MySIMPL code:       " + m.group(1))
      println("    MySIMPLDSL code:    " + convertedCode)
      try {
        val code = "new dsl.ConverterObject { override def result() : Double = { return " + convertedCode + "} }"
        val converterObject : ConverterObject = toolbox.eval(toolbox.parse(code)).asInstanceOf[ConverterObject]
        println("    Expected result:    " + m.group(2))
        println("    Actual result:      " + converterObject.result)
        if(m.group(2).toDouble == converterObject.result) println ("TEST PASS")
        else println("TEST FAIL")
      } catch {
        case _ : Exception => println("CONVERSION FAIL")
      }
    }
  }
  
  // Converter
  def convert(s: String) : String = {
    // First do some cleanup, then convert elements in order of largest to smallest
    var result = s.replaceAll(",", " ").replaceAll(";", "\\|")
    
    // Converting...
    
    // Return statements
    result = convertReturn(result)
        
    // Functions
    result = replace(result, 
      """function (\b[a-zA-Z][a-zA-Z0-9_]*\b) \'\(\' (\b[a-zA-Z][a-zA-Z0-9_]*\b) \'\)\' \'\{\' (.*?) \'\}\'""", 
      "'$1~('$2,p|$3)"
    )
    
    // Loop
    result = replace(result, 
      """while \'\(\' (.*?) \'\)\' do (.*?) done""",
      "([$1])~($2)"
    )
        
    // Conditionals - first do if-then-else, then if-then    
    result = replace(result, 
      """(.*)if \'\(\' (.*?) \'\)\' then (.*?) \'\.\' else (.*?)[^\|]\'\.\' endif(.*)""",
      "$1([$2])?($3 '.',$4 '.')$5"
    )
    
    result = replace(result, 
      """(.*)if \'\(\' (.*?) \'\)\' then (.*?) \'\.\' endif(.*)""",
      "$1([$2])?($3 '.')$4"
    )
    
    // Function calls
    result = replace(result,
      """(\b[a-zA-Z][a-zA-Z0-9_]*\b) \'\(\' (\b[a-zA-Z][a-zA-Z0-9_]*\b) \'\)\'""",
      "('$1~('$2))"
    )
    
    result = replace(result,
      """(\b[a-zA-Z][a-zA-Z0-9_]*\b) \'\(\' (.*?) \'\)\'""",
      "('$1~($2))"
    )
    
    // Non-boolean Conditions
    result = convertConditions(result)
    
    // Expressions
    result = convertExpressions(result)
    
    // DeclAssignment
    // Number
    result = replace(result, 
      """var (\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\d+)""",
      "'$1⟶$2"
    )
    // Variable
    result = replace(result, 
      """var (\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\b[a-zA-Z][a-zA-Z0-9_]*\b)""",
      "'$1⟶'$2"
    )
    // Expression or function call
    result = replace(result, 
      """var (\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\(.*\))""",
      "'$1⟶$2"
    )
    
    // Declarations
    result = replace(result, 
      """var (\b[a-zA-Z][a-zA-Z0-9_]*\b)""",
      "'$1"
    )
    
    // Assignments
    // Number
    result = replace(result, 
      """(\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\d+)""",
      "'$1→$2"
    )
    // Variable
    result = replace(result, 
      """(\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\b[a-zA-Z][a-zA-Z0-9_]*\b)""",
      "'$1→'$2"
    )
    // Expression or function call
    result = replace(result, 
      """(\b[a-zA-Z][a-zA-Z0-9_]*\b) <- (\(.*\))""",
      "'$1→$2"
    )
        
    // Parenthesis for overriding order of operations
    result = result.replace("[", "(").replace("]", ")")
    
    // Final cleanup
    result = result.replace("'.'", "").replace(" ", "")
    
    return "p|" + result + "|GO"
  }
  
  // CONVERT RETURN STATEMENTS
  
  def convertReturn(matchString: String) : String = {
    var oldMatchString : String = ""
    var newMatchString = matchString
    while(newMatchString != oldMatchString) {
      oldMatchString = newMatchString
      newMatchString = convertReturnInternal(newMatchString)
    }
    return newMatchString
  }
  
  def convertReturnInternal(matchString: String) : String = {
    var s : String = matchString
    val expConverter = regexMatch("""(.*)return (.*?) \'\.\'(.*)""".r) ^^ {
      case m => {
        var retValue = m.group(2)
        if(retValue.matches("""(\b[a-zA-Z][a-zA-Z0-9_]*\b)""")) {
          retValue = "'" + retValue
        }
        s = m.group(1) + "r(" + retValue + ")" + m.group(3)
      }
    }
    parseAll(expConverter, s)
    return s
  }
  
  // CONVERT CONDITIONS
  
  def convertConditions(matchString: String) : String = {
    var oldMatchString : String = ""
    var newMatchString = matchString
    while(newMatchString != oldMatchString) {
      oldMatchString = newMatchString
      newMatchString = convertConditionCleanup(newMatchString)
    }
    return newMatchString
  }
  
  def convertConditionCleanup(matchString: String) : String = {
    var retValue = matchString
    // First find the condition, then evaluate it
    val cleanupMatcher = regexMatch("""(.*)\[(.*?)\](.*)""".r) ^^ {
      case m => {
        retValue = m.group(1) + convertConditionsInternal(m.group(2)) + m.group(3)
      }
    }
    parseAll(cleanupMatcher, matchString)
    return retValue
  }
  
  def convertConditionsInternal(matchString: String) : String = {
    // Handle boolean conditions
    if(matchString == "true") return "T"
    if(matchString == "false") return "F"
    
    // Handle not condition
    if(matchString.contains("!")) {
      val notMatcher : String = """! \'\(\' (.*) \'\)\'"""
      val notConverter = regexMatch(notMatcher.r) ^^ {
        case m => {
          val convertedCondition = convertConditionsInternal(m.group(1))
          return "¬(" + convertedCondition + ")"
        }
      }
      parseAll(notConverter, matchString)
    }
    
    // Handle logical conditions
    val logMatcher : String = """\'\(\' (.*) (\|\||\&\&) (.*) \'\)\'"""
    val logConverter = regexMatch(logMatcher.r) ^^ {
      case m => {
        val convertedCondition1 = convertConditionsInternal(m.group(1))
        val convertedCondition2 = convertConditionsInternal(m.group(3))
        return convertedCondition1 + m.group(2) + convertedCondition2
      }
    }
    parseAll(logConverter, matchString)
    
    // Handle comparison conditions
    val compMatcher : String = """(.*) (\=\=|\!\=|\<|\>|\<\=|\>\=) (.*)"""
    if(matchString.matches(compMatcher)) {
        val expConverter = regexMatch(compMatcher.r) ^^ {
        case m => {
          val convertedParam1 = convertExpressionParam(m.group(1))
          val convertedParam2 = convertExpressionParam(m.group(3))
          return convertedParam1 + convertComparator(m.group(2)) + convertedParam2
        }
      }
      parseAll(expConverter, matchString)
    }
    
    return matchString
  }
  
  def convertComparator(c: String) : String = {
    c match {
      case "==" => "≔"
      case "!=" => "≠"
      case "<" => "<"
      case ">" => ">"
      case "<=" => "≤"
      case ">=" => "≥"
    }
  }
  
  // CONVERT EXPRESSIONS
  
  def convertExpressions(matchString: String) : String = {
    var oldMatchString : String = ""
    var newMatchString = matchString
    while(newMatchString != oldMatchString) {
      oldMatchString = newMatchString
      newMatchString = convertExpressionInternal(newMatchString)
    }
    return newMatchString
  }
  
  def convertExpressionInternal(matchString: String) : String = {
    var s : String = matchString
    val expConverter = regexMatch("""(.*)\'\(\' (.*?) ([+-\/*]) (.*?) \'\)\'(.*)""".r) ^^ {
      case m => {
        val convertedParam1 = convertExpressionParam(m.group(2))
        val convertedParam2 = convertExpressionParam(m.group(4))
        s = m.group(1) + "(" + convertedParam1 + " " + m.group(3) + " " + convertedParam2 + ")" + m.group(5)
      }
    }
    parseAll(expConverter, s)
    return s
  }
  
  def convertExpressionParam(paramString: String) : String = {
    var s : String = paramString
    // The parameter is another expression, so we parse it
    if(s.contains("'('") && s.contains("')'")) {
      s = convertExpressions(s)
    }
    // The parameter is either a variable, function call, or a number. If it's a number, we can leave as is
    else if(!s.substring(0, 1).matches("""\d""")) { // Function or variable, add an apostrophe
      val firstChar = s.substring(0, 1)
      if(firstChar != "'" && paramString != "T" && paramString != "F" && paramString != "true" && paramString != "false") {
        s = "'" + s
      }
    }
    return s
  }
  
  // Helper that iteratively replaces the conversion string (for nested loops, conditionals, etc)
  def replace(matchString: String, matchPattern: String, replacePattern: String) : String = {
    var oldMatchString : String = ""
    var newMatchString = matchString
    while(newMatchString != oldMatchString) {
      oldMatchString = newMatchString
      newMatchString = newMatchString.replaceAll(matchPattern, replacePattern)
    }
    return newMatchString
  }
  
  // Helper taken from http://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data
  /** A parser that matches a regex string and returns the Match */
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) => {
          Success(matched, in.drop(start + matched.end - offset))
        }
        case None =>
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }
  
}