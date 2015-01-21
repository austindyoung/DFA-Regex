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

  private def combine(op: (Boolean, Boolean) => Boolean)(dfas: Seq[DFA[AlphabetType]]) = {
    new DFACombiner(this +: dfas, op).combine
  }

  def union(dfas: DFA[AlphabetType]*) = combine(
    (left: Boolean, right: Boolean) => left || right)(dfas)
  def intersect(dfas: DFA[AlphabetType]*) = combine(
    (left: Boolean, right: Boolean) => left && right)(dfas)
  def takeAway(dfas: DFA[AlphabetType]*) = combine(
    (left: Boolean, right: Boolean) => left && !right)(dfas)
  def exteriorProd(dfas: DFA[AlphabetType]*) = combine(
    (left: Boolean, right: Boolean) => left != right)(dfas)

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
  dfas: Iterable[DFA[AlphabetType]],
  operation: (Boolean, Boolean) => Boolean)
    extends CachedStateBuilder[DFAState, DFAState, AlphabetType] {

  type State = DFAState[AlphabetType]

  val combinedAlphabets = dfas.map(
    (dfa) => dfa.alphabet.toSet).reduce((alphabet1, alphabet2) =>
    alphabet1.union(alphabet2)).toIterable

  val sinkState = new LoopDFAState[AlphabetType](false, combinedAlphabets)

  def combine(): DFA[AlphabetType] = {
    val states = mutable.HashSet[DestState]()
    val queue = mutable.Queue[DestState]()
    val startState = getState(dfas.map(dfa => dfa.startState))
    queue.enqueue(startState)
    while(!queue.isEmpty) {
      val current = queue.dequeue()
      states.add(current)
      current.transitionMap.values.foreach(state => {
        if (!states(state)) queue.enqueue(state)
      })
    }
    new DFA[AlphabetType](
      startState,
      states + sinkState,
      Some(combinedAlphabets))
  }

  def buildState(states: Iterable[State]): State = {
    new TransitionMapDFAState[AlphabetType](
      combinedAlphabets.map((alphabetElem) => {
        val destStates = states.map((state: State) =>
          state.transitionMap.get(alphabetElem) match {
            case Some(destState) => destState
            case None => sinkState
          })
        (alphabetElem, getState(destStates))
      }).toMap,
      states.map(state => state.isAcceptState).reduce((left, right) => operation(left, right))
    )
  }
}
