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

  val zero: NFAState[Int] = new TransitionMapNFAState[Int](Map(NonEmpty(0) -> List(zero), NonEmpty(1) -> List(one)), true)
  val one: NFAState[Int] = new TransitionMapNFAState[Int](Map(NonEmpty(0) -> List(zero), NonEmpty(1) -> List(one)), false)
  val endsInZero = new NFA[Int](
    one,
    List(zero, one),
    Epsilon::List(NonEmpty(0), NonEmpty(1)))

  lazy val start: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
    Map(NonEmpty('a') -> List(a)),
    false)
  lazy val a: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
    Map(NonEmpty('b') -> List(b)),
    false)
  lazy val b: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
    Map(),
    true)
  val ab = new NFA[Char](start, List(start, a, b), List(NonEmpty('a'), NonEmpty('b'), Epsilon))

  describe("NFAToDFA") {
    it("works on an NFA that is basically just a DFA") {
      val evenZerosDFA = evenZerosNFA.DFA
      assert(evenZerosDFA.evaluate(List(0, 0)))
      assert(!evenZerosDFA.evaluate(List(0, 1, 0, 1, 0)))
      assert(evenZerosDFA.evaluate(List(0, 1, 0, 1, 0, 0)))
    }
    it("works with non determinism and epsilon transitions") {
      val oneTwoOrThreeFromLastDFA = oneTwoOrThreeFromLastNFA.DFA
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

  describe("Concatenator") {
    it("works on two different NFAs") {
      val theDFA = new Concatenator(endsInZero, oneTwoOrThreeFromLastNFA).concatenate.DFA
      assert(endsInZero.DFA.evaluate(List(1, 0, 1, 0, 0)))
      assert(!theDFA.evaluate(List(1, 0)))
      assert(!theDFA.evaluate(List(1, 0, 0)))
      assert(!theDFA.evaluate(List(1, 0, 0, 0)))
      assert(theDFA.evaluate(List(0, 1, 0)))
      assert(theDFA.evaluate(List(1, 0, 1, 0, 0, 1, 0)))
    }

    it("works on same NFA.") {
      val concatenated = new Concatenator(
        oneTwoOrThreeFromLastNFA, 
        oneTwoOrThreeFromLastNFA).concatenate

      assert(concatenated.evaluate(List(1, 1, 1, 1)))
      val theDFA = new Concatenator(
        oneTwoOrThreeFromLastNFA, 
        oneTwoOrThreeFromLastNFA,
        oneTwoOrThreeFromLastNFA,
        oneTwoOrThreeFromLastNFA).concatenate.DFA
      assert(theDFA.evaluate(List(1, 1, 1, 1, 1, 1, 1, 1)))
      assert(theDFA.evaluate(List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))
      assert(theDFA.evaluate(List(0, 1, 0, 1, 0, 1, 0, 1, 0)))
      assert(theDFA.evaluate(List(1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0)))
      assert(!theDFA.evaluate(List(1, 0, 1, 0, 0)))
      assert(theDFA.evaluate(List(0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0)))
    }
  }

  describe("Kleene Closure") {
    it("recognizes an arbitrary number of repititions.") {
      assert(ab.evaluate("ab"))
      assert(!ab.evaluate("abbb"))
      val abStar = ab *

      assert(abStar.evaluate("abab"))
      assert(!abStar.evaluate("abb"))
      assert(!abStar.evaluate("abbb"))
      assert(abStar.evaluate("abababababababababababababab"))
      assert(abStar.evaluate(""))
    }
    it("doesn't blow up when states point to themselves") {
      lazy val accept: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
        Map(NonEmpty('a') -> List(accept)),
        true)
      val acceptNFA = new NFA(accept, List(accept), List(NonEmpty('a'), Epsilon)) *

      assert(acceptNFA.evaluate("aaaaaa"))
    }
  }

  describe("NFA") {
    it("Can apply concatenation and kleene closure together") {
      val accept = new TransitionMapNFAState[Char](Map(), true)
      lazy val startState: NFAState[Char] = new TransitionMapNFAState[Char](
        Map(
          NonEmpty('a') -> List(startState),
          NonEmpty('b') -> List(startState, secondState)),
        false)
      lazy val secondState: NFAState[Char] = new TransitionMapNFAState[Char](
        Map(
          NonEmpty('a') -> List(thirdState),
          NonEmpty('b') -> List(thirdState),
          Epsilon -> List(thirdState)),
        false)
      lazy val thirdState: NFAState[Char] = new TransitionMapNFAState[Char](
        Map(
          NonEmpty('a') -> List(accept),
          NonEmpty('b') -> List(accept)),
        false)
      val bOneOrTwoFromLastState = new NFA[Char](
        startState,
        List(startState, secondState, thirdState, accept),
        Epsilon::List(NonEmpty('a'), NonEmpty('b')))
      val NFA = ab.*.+(bOneOrTwoFromLastState)
      assert(NFA.evaluate("abba"))
      assert(!NFA.evaluate("ab"))
      assert(NFA.evaluate("ababa"))
      assert(NFA.evaluate("ba"))
    }

    it("doesn't have strange behavior when non-accept start state is reachable") {
      lazy val notB: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
        Map(NonEmpty('a') -> List(notB), NonEmpty('b') -> List(b)),
        false)
      lazy val b: TransitionMapNFAState[Char] = new TransitionMapNFAState[Char](
        Map(NonEmpty('a') -> List(notB), NonEmpty('b') -> List(b)),
        true)
      val endsWithB = new NFA[Char](notB, List(notB, b), List(NonEmpty('a'), NonEmpty('b'), Epsilon))
      assert(!endsWithB.evaluate("a"))
      assert(!endsWithB.evaluate(""))
      assert(endsWithB.evaluate("aaaaaaaab"))
      assert(!endsWithB.*.evaluate("a"))
      assert(endsWithB.*.evaluate(""))
    }
  }
}
