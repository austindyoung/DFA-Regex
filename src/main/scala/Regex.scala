package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Regex[T] {
  def toNFA: NFA[T]
  def toDFA: DFA[T] = toNFA.DFA
}

case class Word[T](content: Seq[T]) extends Regex[T] {
  val addLeft = (letter: T, stateList: List[NFAState[T]]) =>
    new TransitionMapNFAState[T](Map(NonEmpty(letter) -> List(stateList.head)), false) :: stateList;
  def toNFA = {
    val states = content.foldRight(List[NFAState[T]](new TransitionMapNFAState[T](Map(), true)))(addLeft)
    new NFA[T](states.head, states, content.toSet.map((letter: T) => (NonEmpty(letter))))
  }
}
case class Star[T](content: Regex[T]) extends Regex[T] {
  def toNFA = content.toNFA.*
}
case class Union[T](left: Regex[T], right: Regex[T]) extends Regex[T] {
  def toNFA = left.toNFA.union(right.toNFA)
}
case class Concat[T](left: Regex[T], right: Regex[T]) extends Regex[T] {
  def toNFA = left.toNFA.+(right.toNFA)
}


class Parser[T](s: Seq[Either[T,Char]], n: Int,  e: Int, f: Int) {
  var stream = s
  var next = n
  var end = e
  var failed = f

  def max(n: Int, m: Int) = if (n <= m) m else n

  def parse = {
    E(end).getOrElse("syntax error at " ++ failed.toString())
  }

  def E(last: Int): Option[Regex[T]] = {
    if (last < next || last >= stream.size) None
    else {
    val save = next

    val t = T(last)
    if (t == None) {
      failed = max(next, failed)
      next = save
      parseT_OR_E(failed, last)
    }
    else t
  }
  }
  
  def T(last: Int): Option[Regex[T]] = {

    if (last < next || last >= stream.size) None
    else {
    val save = next

    val letter = LETTER(last)
      if (letter == None) {
        failed = max(next, failed)
        next = save
        val letter_star = parseLETTER_STAR(last)
        if (letter_star == None) {
          failed = max(next, failed)
          next = save
          val letter_t = parseLETTER_T(last)
          if (letter_t == None) {
            failed = max(next, failed)
            next = save
            val letter_star_t = parseLETTER_STAR_T(last)
            if (letter_star_t == None) {
              failed = max(next, failed)
              next = save
              val open_e_close = parseOPEN_E_CLOSE(last)
              if (open_e_close == None) {
                failed =  max(next, failed)
                next = save
                val open_e_close_star = parseOPEN_E_CLOSE_STAR(last)
                if (open_e_close_star == None) {
                  failed = max(next, failed)
                  next = save
                  val open_e_close_t_0 = parseOPEN_E_CLOSE_T(failed - 1, last)
                  if (open_e_close_t_0 == None) {
                    failed = max(next, failed)
                    next = save
                    val open_e_close_t_1 = parseOPEN_E_CLOSE_T(failed, last)
                    if (open_e_close_t_1 == None) {
                      failed = max(next, failed)
                      next = save
                      val open_e_close_t_2 = parseOPEN_E_CLOSE_T(failed + 1, last)
                      if (open_e_close_t_2 == None) {
                        failed = max(next, failed)
                        next = save
                        parseOPEN_E_CLOSE_STAR_T(failed, last)
                      }
                      else open_e_close_t_2
                    }
                    else open_e_close_t_1
                  }
                  else open_e_close_t_0
                }
                else open_e_close_star
              }
              else open_e_close
            }
            else letter_star_t
          }
          else letter_t
        }
        else letter_star
      }
      else letter
    }
  }

  def opToken(op: Char) = {
    next = next + 1
    if (next > end || end >= stream.size) false
    else {
    stream(next) match {
      case Left(x) => false
      case Right(char) => char == op
      }
    }
    }
  }

  def OR = opToken('|')

  def OPEN = opToken('(')
  
  def CLOSE = opToken(')')

  def STAR(last: Int) = {
    next = next + 1
    if (next > last || last >= stream.size) false
    else {

    if (next == last) {
      val eitherToken = stream(next)
        eitherToken match {
        case Left(x) => false
        case Right(x) => {
          if (x == '*') true
          else false
        }
        }
    }
    else false
    }
  }

  def LETTER(last: Int) = {
    next = next + 1
    if (next > last || last >= stream.size) None
    else {
    if (next == last) {
      stream(next) match {
        case Left(x) => Some(new Word(List(x)))
        case Right(x) => None
      }
    }
    else None
    }
  }

/** LETTER* */
  def parseLETTER_STAR(last: Int) = {

    val letter = LETTER(last - 1)
    if (letter != None) {
      if (STAR(last)) Some(new Star(letter.get))
      else None
    }
    else None
  }

/** LETTER T */
  def parseLETTER_T(last: Int) = {

    val left = LETTER(next + 1)
    if (left != None) {
      val right = T(last)
      if (right != None) Some(new Concat[T](left.get, right.get))
      else None
    }
    else None
  }

/** LETTER* T */
  def parseLETTER_STAR_T(last: Int) = {

    val letter = LETTER(next + 1)
    if (letter != None) {
      if (STAR(next + 1)) {
        val t = T(last)
        if (t != None)Some(new Concat[T](new Star[T](letter.get), t.get))
        else None
      }
      else None
    }
    else None
  }



/** T | E */
   def parseT_OR_E(fail: Int, last: Int) = {

    val left = T(fail - 1)
    if (left != None) {
      if (OR) {
        val right = E(last)
        if (right != None) Some(new Union(left.get, right.get))
        else None
      }
      else None
    }
    else None
  }

/** (E) */
  def parseOPEN_E_CLOSE(last: Int) = {

    if (OPEN) {
      val inside = E(last - 1)
      if (inside != None) {
        if (CLOSE) inside
        else None
      }
    else  None
    }
    else None
  }

/** (E)* */
  def parseOPEN_E_CLOSE_STAR(last: Int) = {
   
    if (OPEN) {
      val inside = E(last - 2)
      if (inside != None) {
        if (CLOSE) {
          if (STAR(next + 1)) Some(new Star[T](inside.get))
          else None
        }
        else None
      }
      else None
    }
    else None
}

/** (E) T */
  def parseOPEN_E_CLOSE_T(fail: Int, last: Int) = {
    
    val left = parseOPEN_E_CLOSE(fail)
    if (left != None) {
      val t = T(last)
      if (t != None) Some(new Concat[T](left.get, t.get))
      else None
    }
    else None
  }

  
/** (E)* T */
  def parseOPEN_E_CLOSE_STAR_T(fail: Int, last: Int) = {
    
    val left = parseOPEN_E_CLOSE(fail - 1)
    if (left != None) {
      if (STAR(fail)) {
        val t = T(last)
        if (t != None) Some(new Concat[T](new Star[T](left.get), t.get))
        else  None
      }
      else  None
    }
    else None
  }
}

class Parse[T] {
  def parse(exp: Seq[Either[T,Char]]) = (new Parser[T](exp, -1, exp.size - 1, 0)).parse
}

class ParseChar {
  def parseChar(exp: String) = {
    def mapper(token: Char): Either[Char, Char] = if (token != '(' && token != ')' && token != '*' && token != '|') Left(token) else Right(token)
    new Parse[Char].parse(exp.toSeq.map(mapper))
  }
}

object REPLDFA {
  def justChar(character: Char) = {
    lazy val start = new TransitionMapNFAState[Char](
      Map(NonEmpty(character) -> List(accept)),
      false)
    lazy val accept = new TransitionMapNFAState[Char](Map(), true)
    new NFA(start, List(start, accept), List(Epsilon, NonEmpty(character)))
  }

  def main(args: Array[String]) {
    val cabd = (justChar('c') + (justChar('a') + justChar('b')).* + justChar('d')).*
    println("Evaluating an NFA that recognizes the language (c(ab)*d)*")
    var ok = true
    while (ok) {
      val input = readLine()
      ok = input != null
      if (ok) println(cabd.evaluate(input))
    }
  }
}
