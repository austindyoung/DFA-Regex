package com.austinyoung.dfaregex

import org.scalatest.FunSpec

class ParserSpec extends FunSpec {

  def makeWord(string: String) = {
    val first = string.head
    val rest = string.tail
    makeWordAttachedTo(rest, Atom[Char](first))
  }

  def makeWordAttachedTo(string: String, regex: Regex[Char]) = {
    string.foldLeft[Regex[Char]](regex)(
      (regex, char) => Concat(regex, Atom[Char](char))
    )
  }

  describe("Parser") {
    it("parses a two letter word") {
      val testWord = "te"
      assert(RegexStringParser.parse(testWord) == Concat(Atom('t'), Atom('e')))
    }

    it("parses a simple word") {
      val testWord = "test"
      assert(RegexStringParser.parse(testWord) == makeWord(testWord))
    }

    it("parses a union") {
      val testRegexString = "ad|bc"
      val expectedRegex = Union(makeWord("ad"), makeWord("bc"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses kleene star") {
      val testRegexString = "(ad)*"
      val expectedRegex = Star(makeWord("ad"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("applies star only to previous letter without parentheses") {
      val testRegexString = "ing*"
      val expectedRegex = Concat(Concat(Atom('i'), Atom('n')), Star(Atom('g')))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle escapes of *") {
      val testRegexString = "\\*and"
      val expectedRegex = makeWord("*and")
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle nested escapes of reserved characters") {
      val testRegexString = "f\\**|ot"
      val expectedRegex = Union(Concat(Atom('f'), Star(Atom('*'))), Concat(Atom('o'), Atom('t')))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses parentheses") {
      val testRegexString = "(ad)(c)(f)"
      val expectedRegex = Concat(Concat(makeWord("ad"), makeWord("c")), makeWord("f"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses union with lower precedence than anything else") {
      val testRegexString = "a|b*|lf"
      val expectedRegex = Union(Union(Atom('a'), Star(Atom('b'))), Concat(Atom('l'), Atom('f')))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses (1)*1 correctly") {
      val testRegexString = "(1)*1"
      val expectedRegex = Concat(Star(Atom('1')), Atom('1'))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses a complicated expression") {
      val testRegexString = "(a*)|((k*i)|(ob))*|j*h"
      val middle = Star(Union(Concat(Star(Atom('k')), Atom('i')), makeWord("ob")))
      val expectedRegex = Union(Union(Star(Atom('a')), middle), Concat(Star(Atom('j')), Atom('h')))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }
  }
}
