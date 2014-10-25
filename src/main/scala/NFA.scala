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

class TransitionMapNFAState[T](
    _transitionMap: => Map[Alphabet[T], Seq[NFAState[T]]],
    val isAcceptState: Boolean) extends NFAState[T] {
  lazy val transitionMap = _transitionMap
}

class NFA[T](
    val startState: NFAState[T],
    _states: Iterable[NFAState[T]],
    _alphabet: Iterable[Alphabet[T]]) {

  val alphabet = immutable.HashSet[T]() ++ _alphabet
  val states = immutable.HashSet[NFAState[T]]() ++ _states

  def toDFA = {
    new NFAToDFA(this).DFA
  }

  def concatenate(that: NFA[T]) = {
    val (acceptStates, otherStates) = states.partition((state) =>
      state.isAcceptState)

  }
}

class NFAToDFA[T](nfa: NFA[T]) {
  type SourceState = NFAState[T]
  type DestState = DFAState[T]

  private val stateCache: mutable.Map[Iterable[SourceState], DestState] =
    mutable.HashMap[Iterable[SourceState], DestState]()

  def DFA = {
    val states = mutable.HashSet[DestState]()
    val queue = mutable.Queue[DestState]()
    val startState = getState(getEpsilonClosure(List(nfa.startState)))
    queue.enqueue(startState)
    while(!queue.isEmpty) {
      val current = queue.dequeue()
      states.add(current)
      current.transitionMap.values.foreach(state => {
        if (!states(state)) queue.enqueue(state)
      })
    }
    new DFA[T](
      startState,
      states.toSet,
      Some(nfa.alphabet.collect({case NonEmpty(elem: T) => elem}).toIterable))
  }

  def getState(states: Iterable[SourceState]) = {
    stateCache get states match {
      case Some(mappedState) => mappedState
      case None => {
        val newState = buildState(states)
        stateCache.put(states, newState)
        newState
      }
    }
  }

  def buildState(states: Iterable[SourceState]): DestState = {
    TransitionMapDFAState[T](
      () => {
        nfa.alphabet.collect({case element: NonEmpty[T] => {
          (element.value, getState(getClosure(states, element)))
        }}).toMap
      },
      states.exists((state) => state.isAcceptState))
  }

  def getClosure(
      states: Iterable[SourceState],
      element: Alphabet[T]): Iterable[SourceState] = {
    getEpsilonClosure(states.flatMap((state) =>
      (state.transitionMap get element) match {
        case Some(elements) => elements
        case None => List()
      }))
  }

  def getEpsilonClosure(states: Iterable[SourceState]):
      immutable.HashSet[SourceState] = {
    var lastGeneration = states
    var lastStates = immutable.HashSet[SourceState]()
    var thisStates = lastStates ++ states
    do {
      lastGeneration = getEpsilonClosureOneLvl(lastGeneration)
      lastStates = thisStates
      thisStates = thisStates ++ lastGeneration
    } while(lastStates != thisStates)
    thisStates
  }

  private def getEpsilonClosureOneLvl(
      states: Iterable[SourceState]): Iterable[SourceState] = {
    states.flatMap((state) => state.transitionMap get Epsilon match {
      case Some(elements) => elements
      case None => List()
    })
  }
}
