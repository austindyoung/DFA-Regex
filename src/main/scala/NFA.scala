package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Alphabet[+T]

case class NonEmpty[+T](value: T) extends Alphabet[T]

case object Epsilon extends Alphabet[Nothing]

abstract class NFAState[T] {
  def isAcceptState: Boolean
  def transitionMap: Map[Alphabet[T], Iterable[NFAState[T]]]
}

case class TransitionMapNFAState[T](
    transitionMapFunction: () => Map[Alphabet[T], Seq[NFAState[T]]],
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

  type SourceState = NFAState[T]
  type DestState = DFAState[T]

  private val stateCache: mutable.Map[Seq[SourceState], DestState] =
    mutable.HashMap[Seq[SourceState], DestState]()

  def toDFA = {
    //getState(getElementClosure(nfa.startState)
  }

  def getState(states: Seq[SourceState]) = {
    stateCache get states match {
      case Some(mappedState) => mappedState
      case None => {
        val newState = buildState(states)
        stateCache.put(states, newState)
        newState
      }
    }
  }

  def buildState(states: Seq[SourceState]): DestState = {
    LoopDFAState[T](true, List())
  }

  def getClosure(state: NFAState[T], element: Alphabet[T]) = {

  }

  def getClosureOneLevel(state: NFAState[T], element: Alphabet[T]) = {
    val maybeThisState = element match { 
      case NonEmpty(_) => List()
      case Epsilon => List(state)
    }
    (state.transitionMap get element) match {
      case Some(elements) => elements ++ maybeThisState
    }
  }
}

