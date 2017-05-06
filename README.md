# MySIMPLDSL

This project contains a Scala DSL called MySIMPLDSL, based on the MySIMPL language that we implemented in Assignment 2 and Assignment 3 of this course (CS345). The syntax of this language (defined below) has been designed to be as concise as possible, so that it may be used as an imperative "golfing" language. 

Alongside the DSL is a testing/conversion framework (in Converter.scala) that can take existing MySIMPL code in a specific, comma-separated format (examples are provided in Test.scala) and convert it to the new MySIMPLDSL syntax. The framework then uses reflection to test the converted code and compare the output with some provided expected output. Note that there are some known issues with the converter which are enumerated below. 

## Syntax

The following describes the syntax of the MySIMPLDSL language. Note that capital letters indicate parameters. 

### Program
A program of the format `p|[S1|S2|...]|R` is defined where each `S` item in the brackets is a `Statement` separated by a vertical bar `|`, and `R` is a return statement. A program can be executed by appending `|GO`.

### Statements

There are several types of statements:

#### Function Declaration

A function declaration is of the format `S~(V,P)` where `S` is a Symbol defining the function name, `V` is a symbol defining the variable name, and `P` is a program.

#### Variable Declaration

A variable can be declared simply using a Symbol. For example, `'foo` declares a variable named `foo`. 

#### Variable Assignment

A variable assignment statement follows the format `X→E` where `X` is a declared variable and `E` is an expression. For example, `'foo→5` assigns the variable `foo` to a value of 5.

#### Variable Declaration-Assignment

To declare and assign a variable at the same time, simply use the syntax `X⟶E` where `X` and `E` are defined the same way as in the Variable Assignment section above.

#### Conditional Statement

A conditional statement can be either `if-then` or `if-then-else`. 

An `if-then-else` conditional is of the format `(C)?(T,F)` where `C` is a condition, `T` is a sequence of statements to be executed if `C` is true, and `F` is a sequence of statements to be executed if `C` is false.

An `if-then` conditional is of the format `(C)?(T)` where `C` and `T` are defined in the same way. 

#### While Loop

A while loop is of the format `(C)~(S)`, where `C` is a condition and `S` is a sequence of statements to be executed while the condition is true. 

### Conditions

There are several types of conditions that are supported, described below:

#### Comparison Conditions

A comparison condition is of the format `ECE` where `E` is an expression and `C` is a comparator. The following are valid comparators:

- `≔`: equal
- `≠`: not equal
- `<`: less than
- `>`: greater than
- `≤`: less than or equal to
- `≥`: greater than or equal to

#### Logical Conditions

A logical condition is of the format `CLC` where `C` is a condition and `L` is a logical operator. There are two logical operators: AND (`&&`) and OR (`||`). 

#### Booleans

The constant `T` is used to indicate a true condition, while `F` is used to indicate a false condition.

#### Not Conditions

The logical negation of a condition is represented as `¬(C)`, where `C` is the condition.

### Expressions

An expression can be either a variable, a number (in integer or decimal format), a recursive mathematical operation, or a function call. The last two are defined in greater detail below.

### Mathematical Expressions

A mathematical expression is of the format `EME`, where `E` indicates an expression and `M` is a mathematical operator. The following are valid operations:

- `+`: Addition
- `-`: Subtraction
- `*`: Multiplication
- `/`: Division

All operations are performed using the Scala `Double` data type.

### Function Calls

A function call can be made using the syntax `F~(E)` where `F` is the name of the function and `E` is an expression that will be assigned to the variable name that was defined in the function definition.

### Return Statement
A return statement is of the format `r(E)` were `E` is an expression. 

## Sample Code

Below are some simple examples on how to use MySIMPLDSL and its included test framework. 

### Basic Program

```
import dsl._
object Test extends MySIMPLDSL {
  def main(args: Array[String]) {
    println(p|'x|'x→0|('x≥0)?('x→1,'x→2)|r('x)|GO)
  }
}
```

This program gives output `1.0`. 

### Basic Test Case

```
import dsl._
object Test extends MySIMPLDSL {
  def main(args: Array[String]) {
    Test("[var,x,;,x,<-,0,;,if,'(',x,>=,0,')',then,x,<-,1,'.',else,x,<-,2,'.',endif,;,return,x,'.'] | 1.0")
  }
}
```

This program gives the following output:

```
---- Begin Conversion ----
    MySIMPL code:       var,x,;,x,<-,0,;,if,'(',x,>=,0,')',then,x,<-,1,'.',else,x,<-,2,'.',endif,;,return,x,'.'
    MySIMPLDSL code:    p|'x|'x→0|('x≥0)?('x→1,'x→2)|r('x)|GO
    Expected result:    1.0
    Actual result:      1.0
TEST PASS
```

## Known Conversion Issues

Due to time constraints for this project, not all conversion works properly. Here are some known issues.

- Conversion of nested/chained logical operators may not work properly.
- Conversion of chained mathematical operators may not work properly. 
- Conversion of programs that return a function call may not work properly. 
- Other unknown conversion issues may exist.