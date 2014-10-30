package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Regex[T] {
  def toDFA: DFA[T]
}
case class Word[T](content: Seq[T]) extends Regex[T] {
  def toDFA = {
  val op = (letter: T, stateList: List[NFAState[T]]) => (new TransitionMapNFAState[T](Map(letter -> stateList.head), false)) :: stateList
  val states = content.foldRight(List(finalState))(op)
  lazy val finalState: NFAState[T] = new TransitionMapNFAState(Map(),true)
  (new NFA[T](states.head, states, content.toSet.map((letter: T) => (NonEmpty(letter))))).DFA
  }
}
case class *[T](regex: Regex[T]) extends Regex[T] {
  def toDFA = regex.toDFA.*
}
case class U[T](regex1: Regex[T], regex2: Regex[T]) extends Regex[T] {
  def toDFA = regex1.toDFA.union(regex2.toDFA)
}
case class +[T](regex1: Regex[T], regex2: Regex[T]) extends Regex[T] {
  def toDFA = regex1.toDFA.+(regex2.toDFA)
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
