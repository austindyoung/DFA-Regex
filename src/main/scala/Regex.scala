package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Regex[T] {
  def toNFA: NFA[T]
  def toDFA: DFA[T] = toNFA.DFA
}

case class Atom[T](content: T) extends Regex[T] {
  def toNFA = {
    val acceptState = new TransitionMapNFAState[T](Map(), true)
    val startState = new TransitionMapNFAState[T](Map(NonEmpty(content) -> List(acceptState)), false)
    new NFA[T](startState, List(acceptState, startState), Set(NonEmpty(content)))
  }
}

case class Word[T](content: Seq[T]) extends Regex[T] {
  val addLeft = (letter: T, stateList: List[NFAState[T]]) =>
    new TransitionMapNFAState[T](Map(NonEmpty(letter) -> List(stateList.head)), false) :: stateList;
  def toNFA = {
    val states = content.foldRight(List[NFAState[T]](new TransitionMapNFAState[T](Map(), true)))(addLeft)
    new NFA[T](states.head, states, content.toSet.map((letter: T) => (NonEmpty(letter))))
  }
}

abstract class UnaryRegex[T](regex: Regex[T]) extends Regex[T]

case class Star[T](content: Regex[T]) extends UnaryRegex[T](content) {
  def toNFA = content.toNFA.*
}

abstract class BinaryRegex[T](left: Regex[T], right: Regex[T]) extends Regex[T]

case class Union[T](left: Regex[T], right: Regex[T]) extends BinaryRegex[T](left, right) {
  def toNFA = left.toNFA.union(right.toNFA)
}

case class Concat[T](left: Regex[T], right: Regex[T]) extends BinaryRegex[T](left, right) {
  def toNFA = left.toNFA.+(right.toNFA)
}
