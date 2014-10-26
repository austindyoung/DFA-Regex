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
    _transitionMap: => Map[Alphabet[T], Iterable[NFAState[T]]],
    val isAcceptState: Boolean,
    _name: Option[String] = None) extends NFAState[T] {
  lazy val transitionMap = _transitionMap
}

class NFA[T](
    val startState: NFAState[T],
    _states: Iterable[NFAState[T]],
    val alphabet: Iterable[Alphabet[T]],
    _name: Option[String] = None) {

  lazy val DFA = new NFAToDFA(this).DFA

  val states = immutable.HashSet[NFAState[T]]() ++ _states

  def evaluate = this.DFA.evaluate _

  def * = new Kleene(this).kleene

  def +(nfas: NFA[T]*) = new Concatenator(this +: nfas:_*).concatenate
}

trait CachedStateBuilder[Source[_], Dest[_], Alphabet] {
  type SourceState = Source[Alphabet]
  type DestState = Dest[Alphabet]

  val stateCache: mutable.Map[Iterable[SourceState], DestState] =
    mutable.HashMap[Iterable[SourceState], DestState]()

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

  def buildState(states: Iterable[SourceState]): DestState
}

class Kleene[T](nfa: NFA[T]) extends CachedStateBuilder[NFAState, NFAState, T] {

  type State = NFAState[T]

  def kleene = new NFA(
    getState(List(nfa.startState)), 
    nfa.states.map((state) =>
      getState(List(state))),
    nfa.alphabet)

  def buildState(states: Iterable[State]) = buildState(states.head)

  def buildState(state: State): State = {
    val transitionMap = state.transitionMap get Epsilon match {
      case Some(_) => state.transitionMap
      // We need to make sure that Epsilon is in the transition map so
      // we are able to insert the epsilon transition to linkState
      case None => state.transitionMap + (Epsilon -> List())
    }
    val newState: State = new TransitionMapNFAState[T](
      transitionMap.map({case (element, states) => {
        var newStates = states.map((elementState) => stateCache(List(elementState)))
        newStates = element match {
          case a: NonEmpty[T] =>  newStates
          case Epsilon => if (state.isAcceptState) {
            newStates ++ List(stateCache(List(nfa.startState)))
          } else {
            newStates
          }
        }
        (element, newStates)
      }}),
      state.isAcceptState)
    newState
  }
}

class Concatenator[T](nfas: NFA[T]*) {

  type State = NFAState[T]

  // The Int in the key of this map corresponds to the nfas index in
  // nfa array. This is needed to handle the case where some of the
  // concatenated NFA instances have identical (i.e. same reference)
  // NFAStates. This ensures that the new states produced for those
  // Identical NFAStates are different.
  private val oldStateToNewState: mutable.Map[(State, Int), State] =
    mutable.HashMap[(State, Int), State]()

  def concatenate = {
    // Populate the oldStateToNewState with the states of the last nfa
    // involved in the concatenation
    val nfasWithIndex = nfas.zipWithIndex
    val (last, index) = nfasWithIndex.last
    for( state <- last.states ) oldStateToNewState.put((state, index), state)
    nfasWithIndex.reduceRight((left, right) => { linkStates(left, right) })
    val (firstNFA, firstIndex) = nfasWithIndex.head
    new NFA[T](
      oldStateToNewState((firstNFA.startState, firstIndex)),
      oldStateToNewState.values,
      nfas.head.alphabet
    )
  }

  def linkStates(left: (NFA[T], Int), right: (NFA[T], Int)) = {
    val (leftNFA, leftIndex) = left
    val (rightNFA, rightIndex) = right
    for( state <- leftNFA.states ) {
      oldStateToNewState.put(
        (state, leftIndex),
        buildState(
          state,
          leftIndex,
          if(state.isAcceptState) Some(rightNFA.startState) else None))
        }
    left
  }
  
  def buildState(state: State, nfaIndex: Int, linkState: Option[State]): State = {
    val transitionMap = state.transitionMap get Epsilon match {
      case Some(_) => state.transitionMap
      // We need to make sure that Epsilon is in the transition map so
      // we are able to insert the epsilon transition to linkState
      case None => state.transitionMap + (Epsilon -> List())
    }
    new TransitionMapNFAState[T](
      transitionMap.map({case (element, states) => {
        var newStates = states.map((elementState) => 
          oldStateToNewState((elementState, nfaIndex)))
        newStates = element match {
          case a: NonEmpty[T] =>  newStates
          case Epsilon => linkState match {
            case Some(startState) => {
              newStates ++ List(oldStateToNewState((startState, nfaIndex + 1)))
            }
            case None => newStates
          }
        }
        (element, newStates)
      }}),
      false)
  }
}

class NFAToDFA[T](nfa: NFA[T]) extends CachedStateBuilder[NFAState, DFAState, T] {

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

  def buildState(states: Iterable[SourceState]): DestState = {
    new TransitionMapDFAState[T](
        nfa.alphabet.collect({case element: NonEmpty[T] => {
          (element.value, getState(getClosure(states, element)))
        }}).toMap,
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
