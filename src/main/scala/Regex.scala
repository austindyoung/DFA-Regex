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
