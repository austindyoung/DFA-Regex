package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

class RegexState[T](
  _transitionMap: => Map[Regex[T], Iterable[RegexState[T]]],
  var isAcceptState: Boolean) {
  lazy val originalMap = _transitionMap
  var transitionMap: Map[Regex[T], Iterable[RegexState[T]]] = null
  def activate = {
    transitionMap = originalMap
  }
}

class RegexStatesBuilder[T](val nfa: NFA[T]) extends CachedStateBuilder[NFAState, RegexState, T] {

  def states = {
    nfa.states.map(state => getState(List(state)))
  }

  def buildState(states: Iterable[SourceState]): DestState = {
    val state = states.head
    new RegexState[T](
      state.transitionMap.map[Regex[T], Iterable[DestState]](
        {
          case (transition, destinationStates) => {
            val key = transition match {
              case Epsilon => Empty
              case alphabetMember: NonEmpty[T] => Atom(alphabetMember.value)
            }
            (key, destinationStates.map(destinationState => this.getState(List(destinationState))))
          }
        }
      ),
      state.isAcceptState
    )
  }
}

class StateEliminator[T](val nfa: NFA[T]) extends {
  val statesBuilder = new RegexStatesBuilder(nfa)
  val startState = new RegexState[T](Map(Empty -> statesBuilder.getState(List(nfa.startState))), false)
  val endState = new RegexState[T](Map(), true)
  val regexStates = makeRegexStates

  def makeRegexStates = {
    statesBuilder.states
  }
}
