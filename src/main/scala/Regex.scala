package com.austinyoung.dfaregex

import scala.collection._
import scala.collection.immutable

abstract class Regex[T] {
  def toNFA: NFA[T]
  def toTokenSeq: Seq[Token[T]]
  def toDFA: DFA[T] = toNFA.DFA
  def parenthesize(sequence: Seq[Token[T]]) = (Seq(LeftParen) ++ sequence) ++ Seq(RightParen)

  def isAtom(regex: Regex[T]): Boolean = {
    regex match {
      case a: Atom[T] => true
      case _ => false
    }
  }

  def isUnion(regex: Regex[T]): Boolean = {
    regex match {
      case a: Union[T] => true
      case _ => false
    }
  }
}

case class Atom[T](content: T) extends Regex[T] {
  def toNFA = {
    val acceptState = new TransitionMapNFAState[T](Map(), true)
    val startState = new TransitionMapNFAState[T](Map(NonEmpty(content) -> List(acceptState)), false)
    new NFA[T](startState, List(acceptState, startState), Set(NonEmpty(content)))
  }
  def toTokenSeq = Seq(AlphabetMember[T](content))
}

abstract class UnaryRegex[T](regex: Regex[T]) extends Regex[T]

case class Star[T](content: Regex[T]) extends UnaryRegex[T](content) {
  def toNFA = content.toNFA.*
  def toTokenSeq = {
    if(isAtom(content)) {
      content.toTokenSeq ++ Seq(Asterisk)
    } else {
      parenthesize(content.toTokenSeq) ++ Seq(Asterisk)
    }
  }
}

abstract class BinaryRegex[T](left: Regex[T], right: Regex[T]) extends Regex[T]

case class Union[T](left: Regex[T], right: Regex[T]) extends BinaryRegex[T](left, right) {
  def toNFA = left.toNFA.union(right.toNFA)
  def toTokenSeq = left.toTokenSeq ++ Seq(Bar) ++ right.toTokenSeq
}

case class Concat[T](left: Regex[T], right: Regex[T]) extends BinaryRegex[T](left, right) {
  def toNFA = left.toNFA.+(right.toNFA)
  def toTokenSeq = {
    var leftToken = left.toTokenSeq
    var rightToken = right.toTokenSeq
    if(isUnion(left)) leftToken = parenthesize(leftToken)
    if(isUnion(right)) rightToken = parenthesize(rightToken)
    leftToken ++ rightToken
  }
}

case class Empty[T]() extends Regex[T] {
  def toNFA = {
    val onlyState = new TransitionMapNFAState[T](Map(), true)
    new NFA[T](onlyState, List(onlyState), List())
  }
  def toTokenSeq = Seq()
}
