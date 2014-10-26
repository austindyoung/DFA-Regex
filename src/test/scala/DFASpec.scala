package com.austinyoung.dfaregex

import org.scalatest.FunSpec

class DFASpec extends FunSpec {

  val zerosEven: DFAState[Int] = new TransitionMapDFAState[Int](
    Map(0 -> zerosOdd, 1 -> zerosEven),
    true)
  val zerosOdd: DFAState[Int] = new TransitionMapDFAState[Int](
    Map(0 -> zerosEven, 1 -> zerosOdd),
    false)

  val evenZerosDFA = new DFA(zerosEven, List(zerosEven, zerosOdd))

  val onesEven: DFAState[Int] = new TransitionMapDFAState[Int](
    Map(0 -> onesEven, 1 -> onesOdd),
    true)
  val onesOdd: DFAState[Int] = new TransitionMapDFAState[Int](
    Map(0 -> onesOdd, 1 -> onesEven),
    false)

  val evenOnesDFA = new DFA(onesEven, List(onesEven, onesOdd))

  val over = LoopDFAState[Int](false, List(0, 1))

  val lastWasZero = new TransitionMapDFAState[Int](
    Map(0 -> over, 1 -> lastWasOne),
    true)

  val lastWasOne: DFAState[Int] = new TransitionMapDFAState[Int](
    Map(0 -> lastWasZero, 1 -> over),
    true)

  val noConsecutiveStart = new TransitionMapDFAState[Int](
    Map(0 -> lastWasZero, 1 -> lastWasOne),
    true)

  val noConsecutive = new DFA(
    noConsecutiveStart,
    List(noConsecutiveStart, lastWasZero, lastWasOne, over))

  val done = new TransitionMapDFAState[Int](
    Map(0 -> over, 1 -> over),
    true)
  val one = new TransitionMapDFAState[Int](
    Map(0 -> over, 1 -> done),
    false)
  val oh = new TransitionMapDFAState[Int](
    Map(0 -> one, 1 -> over),
    false)
  val startState = new TransitionMapDFAState[Int](
    Map(0 -> over, 1 -> oh),
    false)
  var oneOhOne = new DFA(
    startState,
    List(done, one, oh, startState, over))

  describe("Same number of zeros and ones DFA") {

    it("accepts strings with an even number of zeros.") {
      assert(zerosEven.transition(0) == zerosOdd)
      assert(zerosEven.transition(0).transition(0) == zerosEven)
      assert(evenZerosDFA.evaluate(List(0, 0)))
      assert(!evenZerosDFA.evaluate(List(0, 1, 0, 1, 0)))
      assert(evenOnesDFA.evaluate(List(0, 1, 0, 1, 0)))
      assert(!evenOnesDFA.evaluate(List(0, 1, 0, 1, 1)))
      assert(evenOnesDFA.evaluate(List(0, 1, 0, 1, 1, 1)))
    }
  }
  describe("DFA") {

    it("supports intersect") {
      val bothEvenDFA = evenZerosDFA.intersect(evenOnesDFA)
      assert(bothEvenDFA.evaluate(List(0, 1, 0, 1)))
      assert(!bothEvenDFA.evaluate(List(0, 1, 0, 1, 1, 0)))
      assert(!bothEvenDFA.evaluate(List(0, 1, 0, 1, 0)))
      assert(!bothEvenDFA.evaluate(List(0, 1, 0, 1, 1)))

      val noConsecutiveAndAllEven = bothEvenDFA.intersect(noConsecutive)
      assert(noConsecutiveAndAllEven.evaluate(List(0, 1, 0, 1)))
      assert(!noConsecutiveAndAllEven.evaluate(List(0, 1, 0, 1, 1, 1)))
    }

    it("supports union") {
      val eitherEvenDFA = evenZerosDFA.union(evenOnesDFA)
      assert(eitherEvenDFA.evaluate(List(0, 1, 0, 1)))
      assert(eitherEvenDFA.evaluate(List(0, 1, 0, 1, 1)))
      assert(!eitherEvenDFA.evaluate(List(0, 1, 0, 1, 1, 0)))

      val noConsecutiveAndEitherEven = eitherEvenDFA.intersect(noConsecutive)
      assert(!noConsecutiveAndEitherEven.evaluate(List(1, 0)))
      assert(noConsecutiveAndEitherEven.evaluate(List(1, 0, 1)))
      assert(!noConsecutiveAndEitherEven.evaluate(List(1, 0, 0, 1)))

      val anyCondition = eitherEvenDFA.union(noConsecutive)
      assert(anyCondition.evaluate(List(1, 1, 0)))
      assert(anyCondition.evaluate(List(1, 0)))
    }

    it("converts to an NFA") {
      val eitherEvenNFA = evenZerosDFA.union(evenOnesDFA).NFA.DFA.NFA.DFA
      assert(eitherEvenNFA.evaluate(List(0, 1, 0, 1)))
      assert(eitherEvenNFA.evaluate(List(0, 1, 0, 1, 1)))
      assert(!eitherEvenNFA.evaluate(List(0, 1)))
      assert(!eitherEvenNFA.evaluate(List(0, 1, 0, 1, 1, 0)))
    }

    it("supports concatenation") {
      assert((oneOhOne + oneOhOne).evaluate(List(1, 0, 1, 1, 0, 1)))
    }

    it("supports kleene*") {
      assert(oneOhOne.evaluate(List(1, 0, 1)))
      assert(!oneOhOne.evaluate(List(1, 0, 1, 1)))
      val starred = oneOhOne *

      assert(starred.evaluate(List(1, 0, 1)))
      assert(starred.evaluate(List(1, 0, 1, 1, 0, 1, 1, 0, 1)))
    }
  }
}
