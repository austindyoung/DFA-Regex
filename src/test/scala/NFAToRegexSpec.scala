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
    println("actual:")
    println(regexString)
    println("lol")
    println(regex.toTokenSeq.map({(t: Token[Char]) => t.toString}).mkString)
    println(regex)
    val nfa = regex.toNFA
    inputs.foreach(
      input => {
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
    testStateEliminator("abcd", List("abc", "abcd"))
    testStateEliminator("abcdefg", List("abcdefg", "abc", "aaa", "abcdefg", "aabcdefg"))
  }

  it("handles a union (a|b)") {
    testStateEliminator("a|b", List("a", "b", "aaaaa", "bbbb"))
  }

  it("handles star ((ab)*)") {
    testStateEliminator("(ab)*", List("ab", "aba", "abab", "aabababa", "abababababab", "abab"))
  }

  it("handles nested stars (((ac)*b)*)") {
    testStateEliminator("((ac)*b)*", List("acb", "", "acacacb", "acacab", "accbac", "acacbacb", "bacb"))
  }

  it("handles unions with stars ((ab|c|(jk)*)") {
    testStateEliminator("ab|c|(jk)*", List("ab", "c", "abc", "ajkjkjk", "jkjkjk", "jkjkjkj"))
  }

  it("handles stars of unions ((a|bb)*)") {
    testStateEliminator("(a|bb)*", List("aababaababab", "aaaaa", "bbbb", "abbbbbbabb", "abba"))
  }

  it("handles stars of unions ((111|a(ab*|k))*)") {
    testStateEliminator("(111|a(ab*|k))*", List("111", "111111", "aabk", "aababababak", "111akkkk", "111akaab"))
  }

  it("handles weird bug ((1|a|b|c|e)") {
    testStateEliminator("1|a|b|c|d|e|k|s|i|f|r|i|p|#", List("1", "a", "aa", "b", "c", "f", "r", "i", "p"))
  }

  it("testing") {
    testStateEliminator("((k*i)|(ob))*", List())
  }

  it("handles complicated stuff ((a*)|((k*i)|(ob))*|j*h") {
    testStateEliminator("(a*)|((k*i)|(ob))*|j*h",
                        List("kkkkiobkiiob", "kkkkiobkiiiiobki", "kkkkiobkiiiiobkik",
                             "kkkkiobkiiiiobkikok", "jjjjjjjjjh", "aaaaa"))
  }

}
