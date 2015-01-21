package com.austinyoung.dfaregex
import scala.collection._
import scala.collection.immutable

abstract class Token[+T]
case class AlphabetMember[+T](value: T) extends Token[T]
case object LeftParen extends Token[Nothing]
case object RightParen extends Token[Nothing]
case object Asterisk extends Token[Nothing]
case object Bar extends Token[Nothing]
case object Dot extends Token[Nothing]


/* The grammar used in this parser is roughly as follows
 exp ::= <term><bin>
 bin ::= <bop><term><bin>
 bin ::= ε
 bop ::= |
 bop ::= ·
 bop ::= ε
 term ::= <alph>
 term ::= (<exp>)
 term ::= <term><unary>
 unary ::= * | "~"
 */

case class ParseResult[T](regex: Regex[T], remaining: Seq[Token[T]])

object RegexParser {


  val tokenToBinaryOperator = Map[Token[Nothing], Object](
    Bar -> Union,
    Dot -> Concat
  )

  val operatorPrecedenceTiers = List[Iterable[Token[Nothing]]](
    List(Dot),
    List(Bar)
  )

  val operatorToPrecedence = operatorPrecedenceTiers.zipWithIndex.flatMap(
    {
      case (operators, precedence) => {
        operators.map(operator => operator -> precedence)
      }
    }
  ).toMap
}

class RegexParser[T](s: Seq[Token[T]]) {

  val regexSeq = s
  var remainingSeq = s
  val expressionStack = new mutable.Stack[Regex[T]]()
  val operationStack = new mutable.Stack[Token[Nothing]]()

  def parseExp: ParseResult[T] = {
    expressionStack.push(parseTerm)
    var continue = true
    while(!remainingSeq.isEmpty && continue) {
      val operator = remainingSeq(0) match {
        case Dot => {
          step
          Some(Dot)
        }
        case t: AlphabetMember[T] => {
          Some(Dot)
        }
        case LeftParen => {
          Some(Dot)
        }
        case Bar => {
          step
          Some(Bar)
        }
        case RightParen => {
          None
        }
        case _ => throw new Exception("Unrecognized next token")
      }

      operator match {
        case Some(operator) => parseTermAndApplyOperator(operator)
        case None => continue = false
      }
    }
    ParseResult(finalizeResult, remainingSeq)
  }

  def parseTerm: Regex[T] = {
    var term = remainingSeq(0) match {
      case LeftParen => {
        step
        val result = new RegexParser(remainingSeq).parseExp
        result.remaining(0) match {
          case RightParen => remainingSeq = result.remaining.tail
          case _ => throw new Exception("Closing parenthesis not found")
        }
        result.regex
      }
      case am: AlphabetMember[T] => {
        step
        Atom(am.value)
      }
    }
    var done = false
    while(!done && !remainingSeq.isEmpty) {
      remainingSeq(0) match {
        case Asterisk => {
          step
          term = Star(term)
        }
        case _ => done = true
      }
    }
    term
  }

  def finalizeResult = {
    while(!operationStack.isEmpty) reduceStacks();
    println(expressionStack.head)
    assert(expressionStack.length == 1)
    expressionStack.pop
  }

  def parseTermAndApplyOperator(operator: Token[Nothing]) = {
    while(!operationStack.isEmpty &&
            RegexParser.operatorToPrecedence(operator) >=
            RegexParser.operatorToPrecedence(operationStack.head)) reduceStacks();
    expressionStack.push(parseTerm)
    operationStack.push(operator)
  }

  def reduceStacks() = {
    val right = expressionStack.pop
    val left = expressionStack.pop
    expressionStack.push(
      operationStack.pop match {
        case Bar => Union(left, right)
        case Dot => Concat(left, right)
      }
    )
  }

  def step = {
    remainingSeq = remainingSeq.drop(1)
  }
}

object RegexStringParser {
  def parse(regexString: String) = {
    var escape = false
    var tokenSequence = new mutable.MutableList[Token[Char]]()
    for(char <- regexString) {
      if(escape) {
        tokenSequence += AlphabetMember(char)
        escape = false
      } else {
        tokenForCharacter(char) match {
          case Some(token) => { tokenSequence += token }
          case None => { escape = true }
        }
      }
    }
    val parseResult = new RegexParser(tokenSequence).parseExp
    assert(parseResult.remaining.isEmpty)
    parseResult.regex
  }

  def tokenForCharacter(char: Char):Option[Token[Char]] = {
    char match {
      case '*' => Some(Asterisk)
      case '(' => Some(LeftParen)
      case ')' => Some(RightParen)
      case '·' => Some(Dot)
      case '|' => Some(Bar)
      case '\\' => None
      case _ => Some(AlphabetMember(char))
    }
  }
}
