package com.austinyoung.dfaregex

import org.scalatest.FunSpec

import scala.collection._
import scala.collection.immutable

class RegexStateSpec extends FunSpec {

  it("handles Empty[T] regexes properly") {
    val regexState = new RegexState[Char](mutable.Map[Regex[Char], Set[RegexState[Char]]](), false)
    val anotherState = new RegexState[Char](mutable.Map[Regex[Char], Set[RegexState[Char]]](), false)
    val thirdState = new RegexState[Char](mutable.Map[Regex[Char], Set[RegexState[Char]]](), false)
    regexState.addStateForRegex(Empty(), anotherState)
    regexState.addStateForRegex(Empty(), anotherState)
    assert(regexState.transitionMap(Empty()).contains(anotherState))
    assert(regexState.transitionMap(Empty()).size == 1)
    regexState.removeStateForRegex(Empty(), anotherState)
    assert(regexState.transitionMap.get(Empty()) == None)

    regexState.addStateForRegex(Concat(Atom('a'), Atom('b')), anotherState)
    regexState.addStateForRegex(Concat(Atom('a'), Atom('b')), thirdState)

    assert(regexState.transitionMap(Concat(Atom('a'), Atom('b'))).contains(anotherState))
    assert(regexState.transitionMap(Concat(Atom('a'), Atom('b'))).contains(thirdState))
    regexState.removeStateForRegex(Concat(Atom('a'), Atom('b')), anotherState)
    assert(regexState.transitionMap(Concat(Atom('a'), Atom('b'))).contains(thirdState))
    assert(!regexState.transitionMap(Concat(Atom('a'), Atom('b'))).contains(anotherState))
  }
}

class StateEliminatorSpec extends FunSpec {

  def testStateEliminator(regexString: String, inputs: List[String]) {
    val referenceNFA: NFA[Char] = RegexStringParser.parse(regexString).toNFA
    val regex = new StateEliminator(referenceNFA).regex
    val nfa = regex.toNFA
    println(regex)
    inputs.foreach(
      input => {
        println(input)
        assert(referenceNFA.evaluate(input) == nfa.evaluate(input))
      }
    )
  }

  it("applies binop ignoring empties correctly") {
    val stateEliminator = new StateEliminator(RegexStringParser.parse("random").toNFA)
    assert(stateEliminator.binOpIgnoringEmpties((a, b) => Union(a, b))(Empty(), Atom('a')) == Atom('a'))
    assert(stateEliminator.binOpIgnoringEmpties((a, b) => Union(a, b))(Atom('b'), Empty()) == Atom('b'))
    assert(stateEliminator.binOpIgnoringEmpties((a, b) => Union(a, b))(Atom('b'), Atom('c')) == Union(Atom('b'), Atom('c')))

    val concated = List(Atom('a'), Atom('b'), Atom('c')).reduceLeft(
      stateEliminator.binOpIgnoringEmpties((a, b) => Concat(a, b)) _)

    assert(concated == Concat(Concat(Atom('a'), Atom('b')), Atom('c')))
  }

  it("handles simple words (abcdefg)") {
    println("for regex abcd")
    testStateEliminator("abcd", List("abc", "abcd"))
    println("XXXXXXXX")
    testStateEliminator("abcdefg", List("abcdefg", "abc", "aaa", "abcdefgi", "kabcdefg"))
  }

  it("handles a union (a|b)") {
    testStateEliminator("a|b", List("a", "b", "aaaaa", "bbbb"))
  }

  it("handles star ((ab)*)") {
    testStateEliminator("(ab)*", List("ab", "aba", "abab", "aabababa", "abababababab", "abab"))
  }

  it("handles with union (((ac)*b)*)") {
    testStateEliminator("((ac)*b)*", List("acb", "", "acacacb", "acacab", "accbac", "acacbacb", "bacb"))
  }
}
