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
 bin ::= <bop><term>
 bin ::= <bop><term><bin>
 term ::= <alph>
 term ::= (<exp>)
 term ::= <term><unary>
 unary ::= * | "~"
 bop ::= |
 bop ::= ·
 */

class Parser[T](incoming: Seq[Token[T]]) {
  val regex = incoming

  def parseParenthesized() = {

  }

  def parseExp() = {
    parseTerm()
  }
}

object PreParser {
  def addDots(tokens: Seq[Token[T]]) {
    var result = new mutable.MutableList[Token[Char]]()
    var 
  }
}


object RegexStringParser {
  def parse(regexString: String) = {
    var escape = false
    var result = new mutable.MutableList[Token[Char]]()
    for(char <- regexString) {
      if(escape) {
        result += AlphabetMember(char)
        escape = false
      } else {
        tokenForCharacter(char) match {
          case Some(token) => { result += token }
          case None => { escape = true }
        }
      }
    }
    result
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
