package com.austinyoung.dfaregex

import org.scalatest.FunSpec

class NFASpec extends FunSpec {
  val zerosEven: NFAState[Int] = new TransitionMapNFAState[Int](
    Map(NonEmpty(0) -> List(zerosOdd), NonEmpty(1) -> List(zerosEven)),
    true)
  val zerosOdd: NFAState[Int] = new TransitionMapNFAState[Int](
    Map(NonEmpty(0) -> List(zerosEven), NonEmpty(1) -> List(zerosOdd)),
    false)
  val evenZerosNFA = new NFA[Int](
    zerosEven,
    List(zerosEven, zerosOdd),
    List(NonEmpty(0), NonEmpty(1)))

  val startState: NFAState[Int] = new TransitionMapNFAState[Int](
    Map(
      NonEmpty(0) -> List(startState),
      NonEmpty(1) -> List(startState, secondState)),
    false)
  val secondState: NFAState[Int] = new TransitionMapNFAState[Int](
    Map(
      NonEmpty(0) -> List(thirdState),
      NonEmpty(1) -> List(thirdState),
      Epsilon -> List(thirdState)),
    false)
  val thirdState: NFAState[Int] = new TransitionMapNFAState[Int](
    Map(
      NonEmpty(0) -> List(accept),
      NonEmpty(1) -> List(accept)),
    false)
  val accept = new TransitionMapNFAState[Int](Map(), true)
  val oneTwoOrThreeFromLastNFA = new NFA[Int](
    startState,
    List(startState, secondState, thirdState, accept),
    Epsilon::List(NonEmpty(0), NonEmpty(1)))

  describe("NFAToDFA") {
    it("works on an NFA that is basically just a DFA") {
      val evenZerosDFA = evenZerosNFA.toDFA
      //evenZerosDFA.states.foreach(_ => println(_.transitionMap))
      assert(evenZerosDFA.evaluate(List(0, 0)))
      assert(!evenZerosDFA.evaluate(List(0, 1, 0, 1, 0)))
      assert(evenZerosDFA.evaluate(List(0, 1, 0, 1, 0, 0)))
    }
    it("works with non determinism and epsilon transitions") {
      val oneTwoOrThreeFromLastDFA = oneTwoOrThreeFromLastNFA.toDFA
      assert(!oneTwoOrThreeFromLastDFA.evaluate(List(0, 0)))
      assert(oneTwoOrThreeFromLastDFA.evaluate(List(1, 0)))
      assert(oneTwoOrThreeFromLastDFA.evaluate(List(1, 0, 0)))
      assert(oneTwoOrThreeFromLastDFA.evaluate(List(0, 0, 1, 0, 0)))
      assert(!oneTwoOrThreeFromLastDFA.evaluate(List(0, 0, 1, 0, 0, 0)))
      assert(!oneTwoOrThreeFromLastDFA.evaluate(List(0, 0, 1, 0, 0, 0)))
      assert(oneTwoOrThreeFromLastDFA.evaluate(List(0, 0, 1, 0, 1, 0)))
      assert(oneTwoOrThreeFromLastDFA.evaluate(List(0, 0, 1, 1, 1, 0)))
    }
  }
}
