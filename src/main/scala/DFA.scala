package com.austinyoung.dfaregex

import scala.collection._

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

case class TransitionMapDFAState[AlphabetType](
    transitionMapFunction: () => Map[AlphabetType, DFAState[AlphabetType]],
    isAcceptState: Boolean) extends DFAState[AlphabetType] {
  lazy val transitionMap = transitionMapFunction()
  def transition(alphabetMember: AlphabetType): DFAState[AlphabetType] = 
    this.transitionMap get alphabetMember match {
      case Some(dfaState) => dfaState
      case None => throw UnrecognizedElement(
        "The provided transition element was not recognized.")
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

  def union = combine((left: Boolean, right: Boolean) => left && right)_

  def intersect = combine((left: Boolean, right: Boolean) => left || right)_

  def combine(operation: (Boolean, Boolean) => Boolean)(that: DFA[AlphabetType]) = {
    new DFACombiner(this, that, operation).combine
  }
}


class DFACombiner[AlphabetType](
    left: DFA[AlphabetType],
    right: DFA[AlphabetType],
    operation: (Boolean, Boolean) => Boolean) {

  type State = DFAState[AlphabetType]

  lazy val combinedAlphabets = left.alphabet ++ right.alphabet
  lazy val rightUnrecognized = LoopDFAState[AlphabetType](false, right.alphabet)
  lazy val leftUnrecognized = LoopDFAState[AlphabetType](false, left.alphabet)

  private val stateCache: mutable.Map[(State, State), State] =
    mutable.HashMap[(State, State), State]()

  def combine(): DFA[AlphabetType] = {
    val newStates = left.states.flatMap(
      (leftState: State) => right.states.map(
        (rightState: State) => getState(leftState, rightState)))
    DFA(
      getState(left.startState, right.startState),
      newStates,
      left.alphabet)

  }

  def getState(leftState: State, rightState: State): State = {
    stateCache get (leftState, rightState) match {
      case Some(mappedState) => mappedState
      case None => {
        val newState = buildState(leftState, rightState)
        stateCache.put((leftState, rightState),
          buildState(leftState, rightState))
        newState
      }
    }
  }

  def buildState(leftState: State, rightState: State): State = {
    TransitionMapDFAState[AlphabetType](
      () => combinedAlphabets.map((alphabetElem: AlphabetType) => {
        val newLeftState = leftState.transitionMap.get(alphabetElem) match {
          case Some(newState) => newState
          case None => leftUnrecognized
        }
        val newRightState = rightState.transitionMap.get(alphabetElem) match {
          case Some(newState) => newState
          case None => rightUnrecognized
        }
        (alphabetElem, this.getState(newLeftState, newRightState))
      }).toMap,
      this.operation(leftState.isAcceptState, rightState.isAcceptState)
    )
  }
}
