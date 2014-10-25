package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

class Epsilon;

abstract class NFAState[T] {
  def isAcceptState: Boolean
  def transitionMap: Map[Either[T, Epsilon], Iterable[NFAState[T]]]
}

case class TransitionMapNFAState[T](
    transitionMapFunction: () => Map[Either[T, Epsilon], Iterable[NFAState[T]]],
    isAcceptState: Boolean) extends NFAState[T] {
  lazy val transitionMap = transitionMapFunction()
}

class NFA[AlphabetType](
    val startState: NFAState[AlphabetType],
    _states: Iterable[NFAState[AlphabetType]],
    _alphabet: Iterable[AlphabetType]) {

  val alphabet = immutable.HashSet[AlphabetType]() ++ _alphabet
  val states = immutable.HashSet[NFAState[AlphabetType]]() ++ _states

  def toDFA:DFA[AlphabetType] = {
    

  }
}

