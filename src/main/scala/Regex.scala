package com.austinyoung.dfaregex

import scala.collection.immutable.HashSet

abstract class Regex[T]
case class Word[T](content: Seq[T]) extends Regex[T]
case class Lang[T](language: HashSet[Seq[T]]) extends Regex[T]
case class *[T](regex: Regex[T]) extends Regex[T] 
case class U[T](regex1: Regex[T], regex2: Regex[T]) extends Regex[T]
case class o[T](regex1: Regex[T], regex2: Regex[T]) extends Regex[T]

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
