package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

class RegexState[T](
  _transitionMap: => mutable.Map[Regex[T], Set[RegexState[T]]],
  var isAcceptState: Boolean) {
  lazy val transitionMap = _transitionMap

  def addStateForRegex(regex: Regex[T], state: RegexState[T]) {
    this.transitionMap.put(
      regex,
      this.transitionMap.getOrElse(regex, Set[RegexState[T]]()) + state
    )
  }

  def containsStateForRegex(regex: Regex[T], state: RegexState[T]) {
    this.transitionMap.getOrElse(regex, Set[RegexState[T]]()).contains(state)
  }

  def removeStateForRegex(regex: Regex[T], state: RegexState[T]) {
    val newRegexTransitions = this.transitionMap(regex) - state
    if(newRegexTransitions.isEmpty) {
      this.transitionMap.remove(regex)
    } else {
      this.transitionMap.put(regex, newRegexTransitions)
    }
  }
}

class RegexStatesBuilder[T](val nfa: NFA[T]) extends CachedStateBuilder[NFAState, RegexState, T] {

  def states = {
    nfa.states.map(state => getState(List(state)))
  }

  def buildState(states: Iterable[SourceState]): DestState = {
    val state = states.head
    new DestState(
      buildTransitionMap(state.transitionMap),
      state.isAcceptState
    )
  }

  def buildTransitionMap(nfaTransitionMap: Map[Alphabet[T], Iterable[NFAState[T]]]): mutable.Map[Regex[T], Set[RegexState[T]]] = {
    val immutable = nfaTransitionMap.map(remapTransitionMapPair _)
    mutable.Map(immutable.toSeq: _*)
  }

  def remapTransitionMapPair(pair: (Alphabet[T], Iterable[NFAState[T]])) = {
    val (transition, destinationStates) = pair
    val key = transition match {
      case Epsilon => new Empty[T]()
      case alphabetMember: NonEmpty[T] => Atom(alphabetMember.value)
    }
    key -> destinationStates.map(destinationState => this.getState(List(destinationState))).toSet
  }
}

class StateEliminator[T](val nfa: NFA[T]) extends {
  val statesBuilder = new RegexStatesBuilder(nfa)
  val startState = new RegexState[T](mutable.Map(Empty[T]() -> Set(statesBuilder.getState(List(nfa.startState)))), false)
  val endState = new RegexState[T](mutable.Map(), true)
  val regexStates = statesBuilder.states
  regexStates.foreach(
    state => {
      if(state.isAcceptState) {
        state.addStateForRegex(new Empty(), endState)
        state.isAcceptState = false
      }
    }
  )
  checkStateIntegrity(startState::endState::regexStates.toList)

  def regex = {
    eliminateStates(regexStates.toList)
  }

  def checkStateIntegrity(allStates: List[RegexState[T]]) = {
    for {
      state <- allStates
      (_, transitionStates) <- state.transitionMap
      transitionState <- transitionStates
    } yield {
      assert(allStates.contains(transitionState))
    }
  }

  def eliminateStates(remainingStates: List[RegexState[T]]): Regex[T] = {
    remainingStates match {
      case stateToEliminate::otherStates => {
        eliminateState(stateToEliminate, otherStates)
        val allStates = startState::endState::otherStates
        checkStateIntegrity(allStates)
        eliminateStates(otherStates)
      }
      case Nil => {
        startState.transitionMap.keys.reduce((left, right) => Union(left, right))
      }
    }
  }

  def eliminateState(stateToEliminate: RegexState[T], otherStates: List[RegexState[T]]) = {
    val repetitionRegex = getRepetitionRegex(stateToEliminate)
    for {
      sourceState <- startState::otherStates
      (leftRegex, transitionMapDestinations) <- sourceState.transitionMap
      if transitionMapDestinations.contains(stateToEliminate)
    } yield {
      // Get rid of the stateToEliminate in the map
      sourceState.removeStateForRegex(leftRegex, stateToEliminate)
      stateToEliminate.transitionMap.foreach(
        {
          case (rightRegex, eliminatedDestinationStates) => {
            val regexKey = List(leftRegex, repetitionRegex, rightRegex).reduceLeft(
              binOpIgnoringEmpties((a, b) => Concat(a, b)) _)
            val validDestinationStates = Set(otherStates: _*) + endState
            for(destinationState <- eliminatedDestinationStates
                if validDestinationStates.contains(destinationState)) sourceState.addStateForRegex(regexKey, destinationState)
          }
        }
      )
      sourceState
    }
  }

  def binOpIgnoringEmpties(binOp: (Regex[T], Regex[T]) => Regex[T])(left: Regex[T], right: Regex[T]) = {
    left match {
      case l:Empty[T] => right
      case _ => right match {
        case r:Empty[T] => left
        case _ => binOp(left, right)
      }
    }
  }

  def getRepetitionRegex(stateToEliminate: RegexState[T]) = {
    val repetitionRegexes: Iterable[Regex[T]] = for {
      (regex, destinations) <- stateToEliminate.transitionMap
      if destinations.contains(stateToEliminate)
    } yield regex
    if(repetitionRegexes.isEmpty) Empty[T]() else
      Star(repetitionRegexes.tail.foldLeft(repetitionRegexes.head)(
        (a, b) => Union(a, b)
      ))
  }

}
