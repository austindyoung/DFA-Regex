package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

/** Either[] supports the flexibility of allowing letters not of type Char */
class Parser[T](s: Seq[Either[T, Char]]) {
  
  /** inout stream */
  var stream = s
  
  /** current location in the stream */ 
  var next = -1
  var end = stream.size - 1
  
  /** most advanced location at which the parser had failed */
  var failed = 0

  def max(n: Int, m: Int) = if (n <= m) m else n

  def parse = {
    E(end).getOrElse("syntax error at " ++ failed.toString())
  }

  /** first set of rules in the context-free grammar */
  def E(last: Int): Option[Regex[T]] = {
    if (last < next || last >= stream.size) None
    else {
      val save = next
      val t = T(last)
      if (t == None) {
        failed = max(next, failed)
        next = save
        /** names of this, and the subsequent, 'parse' methods represnet the syntactic form being checked for in the context-free grammar */
        parseT_OR_E(last)
      }
      else t
    }
  }
  
  /** second set of rules */
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
                        parseOPEN_E_CLOSE_STAR_T(last)
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

  def OR = opToken('|')

  def OPEN = opToken('(')
  
  def CLOSE = opToken(')')

  def STAR = opToken('*')

/** methods return None when the input stream does not have the syntactic form being checked for */ 
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
      if (STAR) Some(new Star(letter.get))
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
      if (STAR) {
        val t = T(last)
        if (t != None)Some(new Concat[T](new Star[T](letter.get), t.get))
        else None
      }
      else None
    }
    else None
  }



/** T | E */
   def parseT_OR_E(last: Int) = {

    val left = T(failed - 1)
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
          if (STAR) Some(new Star[T](inside.get))
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
  def parseOPEN_E_CLOSE_STAR_T(last: Int) = {
    
    val left = parseOPEN_E_CLOSE(failed - 1)
    if (left != None) {
      if (STAR) {
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
  def parse(exp: Seq[Either[T, Char]]) = (new Parser[T](exp)).parse
}

class ParseChar {
  def parseChar(exp: String) = {
    def mapper(token: Char): Either[Char, Char] = if (token != '(' && token != ')' && token != '*' && token != '|') Left(token) else Right(token)
    new Parse[Char].parse(exp.toSeq.map(mapper))
  }
}
