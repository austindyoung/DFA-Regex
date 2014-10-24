package com.austinyoung.dfaregex

import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfter

class DFASpec extends FunSpec {

  lazy val zerosEven: DFAState[Int] = new TransitionMapDFAState[Int](
    () => Map(
      0 -> zerosOdd,
      1 -> zerosEven),
    true)
  lazy val zerosOdd: DFAState[Int] = new TransitionMapDFAState[Int](
    () => Map(
      0 -> zerosEven,
      1 -> zerosOdd),
    false)

  val evenZerosDFA = DFA(zerosEven, List(zerosEven, zerosOdd), List(0, 1))

  lazy val onesEven: DFAState[Int] = new TransitionMapDFAState[Int](
      () => Map(
        0 -> onesEven,
        1 -> onesOdd),
      true)
  lazy val onesOdd: DFAState[Int] = new TransitionMapDFAState[Int](
    () => Map(
      0 -> onesOdd,
      1 -> onesEven),
    false)

  val evenOnesDFA = DFA(onesEven, List(onesEven, onesOdd), List(0, 1))

  lazy val over = LoopDFAState[Int](false, List(0, 1))

  lazy val lastWasZero = new TransitionMapDFAState[Int](
    () => Map(
      0 -> over,
      1 -> lastWasOne),
    true)

  lazy val lastWasOne: DFAState[Int] = new TransitionMapDFAState[Int](
    () => Map(
      0 -> lastWasZero,
      1 -> over),
    true)

  val noConsecutiveStart = new TransitionMapDFAState[Int](
    () => Map(
      0 -> lastWasZero,
      1 -> lastWasOne),
    true)

  val noConsecutive = DFA(
    noConsecutiveStart,
    List(noConsecutiveStart, lastWasZero, lastWasOne),
    List(0, 1))

  describe("Same number of zeros and ones DFA") {

    it("accepts strings with an even number of zeros.") {
      assert(zerosEven.transition(0) == zerosOdd)
      assert(zerosEven.transition(0).transition(0) == zerosEven)
      assert(evenZerosDFA.evaluate(List(0, 0)) == true)
      assert(evenZerosDFA.evaluate(List(0, 1, 0, 1, 0)) == false)
      assert(evenOnesDFA.evaluate(List(0, 1, 0, 1, 0)) == true)
      assert(evenOnesDFA.evaluate(List(0, 1, 0, 1, 1)) == false)
      assert(evenOnesDFA.evaluate(List(0, 1, 0, 1, 1, 1)) == true)
    }
  }
  describe("DFA") {

    it("supports unions") {
      val bothEvenDFA = evenZerosDFA.union(evenOnesDFA)
      assert(bothEvenDFA.evaluate(List(0, 1, 0, 1)) == true)
      assert(bothEvenDFA.evaluate(List(0, 1, 0, 1, 1, 0)) == false)
      assert(bothEvenDFA.evaluate(List(0, 1, 0, 1, 0)) == false)
      assert(bothEvenDFA.evaluate(List(0, 1, 0, 1, 1)) == false)

      val noConsecutiveAndAllEven = bothEvenDFA.union(noConsecutive)
      assert(noConsecutiveAndAllEven.evaluate(List(0, 1, 0, 1)) == true)
      assert(noConsecutiveAndAllEven.evaluate(List(0, 1, 0, 1, 1, 1)) == false)
    }

    it("supports intersection") {
      val eitherEvenDFA = evenZerosDFA.intersect(evenOnesDFA)
      assert(eitherEvenDFA.evaluate(List(0, 1, 0, 1)) == true)
      assert(eitherEvenDFA.evaluate(List(0, 1, 0, 1, 1)) == true)
      assert(eitherEvenDFA.evaluate(List(0, 1, 0, 1, 1, 0)) == false)

      val noConsecutiveAndEitherEven = eitherEvenDFA.union(noConsecutive)
      assert(noConsecutiveAndEitherEven.evaluate(List(1, 0)) == false)
      assert(noConsecutiveAndEitherEven.evaluate(List(1, 0, 1)) == true)
      assert(noConsecutiveAndEitherEven.evaluate(List(1, 0, 0, 1)) == false)

      val anyCondition = eitherEvenDFA.intersect(noConsecutive)
      assert(anyCondition.evaluate(List(1, 1, 0)) == true)
      assert(anyCondition.evaluate(List(1, 0)) == true)
    }

  }
}

