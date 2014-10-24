package com.austinyoung.dfaregex

import scala.collection._

case class MissingTransition(smth: String) extends Exception

abstract class DFAState[AlphabetType] {
  def transition(alphabetMember: AlphabetType): DFAState[AlphabetType]
  def isAcceptState: Boolean
  def transitionMap: Map[AlphabetType, DFAState[AlphabetType]]
}

case class LoopDFAState[AlphabetType](isAcceptState: Boolean)
    extends DFAState[AlphabetType] {
  def transition(alphabetMember: AlphabetType) = this
  val transitionMap = Map[AlphabetType, DFAState[AlphabetType]]()
}

case class TransitionMapDFAState[AlphabetType](
    transitionMapFunction: () => Map[AlphabetType, DFAState[AlphabetType]],
    isAcceptState: Boolean) extends DFAState[AlphabetType] {
  lazy val transitionMap = transitionMapFunction()
  def transition(alphabetMember: AlphabetType): DFAState[AlphabetType] = 
    this.transitionMap get alphabetMember match {
      case Some(dfaState) => dfaState
      case None => new LoopDFAState[AlphabetType](false)
    }
}

case class DFA[AlphabetType](
    startState: DFAState[AlphabetType],
    states: Iterable[DFAState[AlphabetType]],
    alphabet: Iterable[AlphabetType]) {
  def evaluate(word: Seq[AlphabetType]) = {
    word.foldLeft(startState)(
      (currentState: DFAState[AlphabetType], elem: AlphabetType) => {
      currentState.transition(elem)
    }).isAcceptState
  }

  def union(that: DFA[AlphabetType]) = combine(that, (left: Boolean, right: Boolean) => left && right)

  def combine(that: DFA[AlphabetType], operation: (Boolean, Boolean) => Boolean) = {
    new DFACombiner(this, that, operation).combine
  }
}


class DFACombiner[AlphabetType](
    left: DFA[AlphabetType],
    right: DFA[AlphabetType],
    operation: (Boolean, Boolean) => Boolean) {

  private val stateCache: mutable.Map[(DFAState[AlphabetType], DFAState[AlphabetType]), DFAState[AlphabetType]] = mutable.HashMap[(DFAState[AlphabetType], DFAState[AlphabetType]), DFAState[AlphabetType]]()
  def combine(): DFA[AlphabetType] = {
    val newStates = left.states.flatMap(
      (leftState: DFAState[AlphabetType]) => right.states.map(
        (rightState: DFAState[AlphabetType]) => getState(leftState, rightState)))
    DFA(
      getState(left.startState, right.startState),
      newStates,
      left.alphabet)

  }

  def getState(
      leftState: DFAState[AlphabetType],
      rightState: DFAState[AlphabetType]): DFAState[AlphabetType] = {
    stateCache get (leftState, rightState) match {
      case Some(mappedState) => mappedState
      case None => {
        val newState = buildState(leftState, rightState)
        stateCache.put((leftState, rightState), buildState(leftState, rightState))
        newState
      }
    }
  }

  def buildState(
      leftState: DFAState[AlphabetType],
      rightState: DFAState[AlphabetType]): DFAState[AlphabetType] = {
    TransitionMapDFAState[AlphabetType](
      () => leftState.transitionMap.map(
        {case (alphabetMember, newLeftState) => {
          rightState.transitionMap.get(alphabetMember) match {
            case Some(newRightState) => (alphabetMember, this.getState(newLeftState, newRightState))
            case None => throw MissingTransition(s"Attempted to combine states with different transition alphabets. $alphabetMember was not present.")
          }
        }}
      ).toMap,
      this.operation(leftState.isAcceptState, rightState.isAcceptState)
    )
  }
}
