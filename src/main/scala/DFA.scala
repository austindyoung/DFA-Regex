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
    assert(state.transitionMap.keySet == alphabet);
    // TODO(@IvanMalison) Why does this cause union to fail?
    // state.transitionMap.foreach({
    //   case (_, mappedState: DFAState[AlphabetType]) =>
    //     assert(states(mappedState))
    // })
  })

  def evaluate(word: Seq[AlphabetType]) = {
    word.foldLeft(startState)(
      (currentState: DFAState[AlphabetType], elem: AlphabetType) => {
      currentState.transition(elem)
    }).isAcceptState
  }

  def union = combine((left: Boolean, right: Boolean) => left || right)_

  def intersect = combine((left: Boolean, right: Boolean) => left && right)_
  
  def takeAway = combine((left: Boolean, right: Boolean) => left && !right)_
  
  def exteriorProd = combine((left: Boolean, right: Boolean) => (left && !right) || (!left && right))_

  def combine(op: (Boolean, Boolean) => Boolean)(that: DFA[AlphabetType]) = {
    new DFACombiner(this, that, op).combine
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
    val newStates = (left.states ++ List(leftUnrecognized)).flatMap(
      (leftState: State) => (right.states ++ List(rightUnrecognized)).map(
        (rightState: State) => getState(leftState, rightState)))
    new DFA(
      getState(left.startState, right.startState),
      newStates,
      Some(left.alphabet ++ right.alphabet))

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
