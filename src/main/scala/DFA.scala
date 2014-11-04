package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

case class MissingTransition(smth: String) extends Exception
case class UnrecognizedElement(smth: String) extends Exception

abstract class DFAState[AlphabetType] {
  def transition(alphabetMember: AlphabetType): DFAState[AlphabetType]
  def isAcceptState: Boolean
  def transitionMap: Map[AlphabetType, DFAState[AlphabetType]]
}

case class LoopDFAState[AlphabetType](
    isAcceptState: Boolean,
    alphabet: Iterable[AlphabetType]) extends DFAState[AlphabetType] {
  def transition(alphabetMember: AlphabetType) = this
  val transitionMap = alphabet.map((alphabetElem: AlphabetType) =>
    (alphabetElem, this)).toMap
}

class TransitionMapDFAState[AlphabetType](
    _transitionMap: => Map[AlphabetType, DFAState[AlphabetType]],
    val isAcceptState: Boolean) extends DFAState[AlphabetType] {
  lazy val transitionMap = _transitionMap
  def transition(alphabetMember: AlphabetType): DFAState[AlphabetType] = 
    this.transitionMap get alphabetMember match {
      case Some(dfaState) => dfaState
      case None => throw UnrecognizedElement(
        "The provided transition element was not recognized.")
    }
}

class DFA[AlphabetType](
    val startState: DFAState[AlphabetType],
    _states: Iterable[DFAState[AlphabetType]],
    _alphabet: Option[Iterable[AlphabetType]] = None) {

  val alphabet = immutable.HashSet[AlphabetType]() ++ (_alphabet match {
    case Some(alphabet) => alphabet: Iterable[AlphabetType]
    case None => startState.transitionMap.keys
  })
  val states = immutable.HashSet[DFAState[AlphabetType]]() ++ _states

  states.foreach((state: DFAState[AlphabetType]) => {
    // Make sure that the keyset of each state is the DFAs alphabet.
    assert(state.transitionMap.keySet == alphabet);
    // Make sure that all of the mapped states are in the DFAs state map.
    state.transitionMap.foreach({
      case (_, mappedState: DFAState[AlphabetType]) =>
        assert(states(mappedState))
    })
  })

  def evaluate(word: Seq[AlphabetType]) = {
    word.foldLeft(startState)(
      (currentState: DFAState[AlphabetType], elem: AlphabetType) => {
        val nextState = currentState.transition(elem)
        nextState
    }).isAcceptState
  }

  private def combine(op: (Boolean, Boolean) => Boolean)(that: DFA[AlphabetType]) = {
    new DFACombiner(this, that, op).combine
  }

  def union(dfas: DFA[AlphabetType]*) = new DFAArbitraryArgCombiner((left: Boolean, right: Boolean) => left || right, this, dfas).arbitraryCombine()
  def intersect = combine((left: Boolean, right: Boolean) => left && right)_
  def takeAway = combine((left: Boolean, right: Boolean) => left && !right)_
  def exteriorProd = combine((left: Boolean, right: Boolean) => left != right)_

  lazy val NFA = new DFAToNFA(this).NFA

  def +(dfas: DFA[AlphabetType]*) =
    NFA.+(dfas.map((dfa) => dfa.NFA):_*).DFA

  def * = this.NFA.*.DFA
}

class DFAToNFA[T](dfa: DFA[T]) extends CachedStateBuilder[DFAState, NFAState, T] {

  def NFA = {
    val startState = getState(List(dfa.startState))
    val alphabet: Iterable[Alphabet[T]] =
      dfa.alphabet.map((elem) => NonEmpty(elem)) ++ List(Epsilon)
    new NFA[T](
      startState,
      dfa.states.map((state) => getState(List(state))),
      alphabet)
  }

  def buildState(states: Iterable[SourceState]) = {
    val state: SourceState = states.head
    new TransitionMapNFAState[T](
      state.transitionMap.map({case (key, mapState: SourceState) =>
        (NonEmpty(key), List(getState(List(mapState))))}),
      state.isAcceptState)
  }
}

class DFACombiner[AlphabetType](
    dfas: List[DFA[AlphabetType]],
    operation: (Boolean, Boolean) => Boolean) extends CachedStateBuilder {

  type State = DFAState[AlphabetType]

  lazy val combinedAlphabets = dfas.map((dfa: DFA[AlphabetType]) => dfa.alphabet.toSet).reduce((alphabet1: Set[AlphabetType], alphabet2: Set[AlphabetType]) => alphabet1.union(alphabet2)).toIterable

  lazy val statesUnrecognizedMap = dfas.map((dfa: DFA[AlphabetType]) => (dfa, new LoopDFAState[AplhabetType](false, dfa.alphabet))).toMap

  def combine(): DFA[AlphabetType] = {
    val cartesianProd = (dfa: DFA[AlphabetType], productsSet: List[List[State]]) => (dfa.states ++ List(new LoopDFAState[AlphabetType](false, dfa.alphabet))).flatMap(
      (leftState: State) => productsSet.map((product: List[State]) => leftState :: product))
    val newStates = dfas.foldRight(Nil, cartesianProd).map(getState(_))
    new DFA(
      getState(dfas.map((dfa: DFA[AlphabetType]) => dfa.startState)),
      newStates,
      combinedAlphabets)
  }

  def buildState(states: Iterable[State]): State = {
        new TransitionMapDFAState[AlphabetType](
          combinedAlphabets.map((alphabetElem: AlphabetType) => {
            val destStates = states.map((state: State) => state.transitionMap.get(alphabetElem) match {
              case Some(destState) => destState
              case None => statesUnrecognizedMap.get(destState).get
            })
            (alphabetElem, this.getState(destStates))
          }).toMap,
          destStates.reduce(this.operation))
          }
}


