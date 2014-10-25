package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Alphabet[T];

case class NonEmpty[T](value: T) extends Alphabet[T]

case object Epsilon extends Alphabet

// class Epsilon {
// }

abstract class NFAState[T] {
  def isAcceptState: Boolean
  def transitionMap: Map[Either[T, Epsilon], Iterable[NFAState[T]]]
}

case class TransitionMapNFAState[T](
    transitionMapFunction: () => Map[Either[T, Epsilon], Iterable[NFAState[T]]],
    isAcceptState: Boolean) extends NFAState[T] {
  lazy val transitionMap = transitionMapFunction()
}

class NFA[T](
    val startState: NFAState[T],
    _states: Iterable[NFAState[T]],
    _alphabet: Iterable[T]) {

  val alphabet = immutable.HashSet[T]() ++ _alphabet
  val states = immutable.HashSet[NFAState[T]]() ++ _states

}

class NFAToDFA[T](nfa: NFA[T]) {
  type Alphabet = Either[T, Epsilon]
  def toDFA = {
    //getState(getElementClosure(nfa.startState)

  }

  def getClosure(state: NFAState[T], element: Alphabet) {

  }

  def getClosureOneLevel(state: NFAState[T], element: Alphabet) {
    val maybeThisState = element match { 
      case Right(_) => List()
      case Left(_) => List(state)
    }
    (state.transitionMap get element) match {
      case Some(elements) => elements ++ maybeThisState
      case None => maybeThisState
    }
  }
}

