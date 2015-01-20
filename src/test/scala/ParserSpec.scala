package com.austinyoung.dfaregex

import org.scalatest.FunSpec

class ParserSpec extends FunSpec {

  def makeWord(string: String) {
    string.map(_ => Atom(_))
  }

  describe("Parser") {
    it("parses a simple word") {
      val testWord = "testWord"
      assert(RegexStringParser.parse(testWord) == Word(testWord))
    }

    it("parses a union") {
      val testRegexString = "ad|bc"
      val expectedRegex = Union(Word("ad"), Word("bc"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses kleene star") {
      val testRegexString = "(ad)*"
      val expectedRegex = Star(Word("ad"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("applies star to entire previous word even without parentheses") {
      val testRegexString = "something*"
      val expectedRegex = Star(Word("something"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle escapes of *") {
      val testRegexString = "\\*andsomestuff"
      val expectedRegex = Word("*andsomestuff")
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle escapes of )") {
      val testRegexString = "(\\)andsomestuff)"
      val expectedRegex = Word(")andsomestuff")
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle escapes of (") {
      val testRegexString = "(\\(andsomestuff)"
      val expectedRegex = Word("(andsomestuff")
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("can handle nested escapes of reserved characters") {
      val testRegexString = "stuff\\**|other"
      val expectedRegex = Union(Star(Word("stuff*")), Word("other"))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses parentheses") {
      val testRegexString = "(ad)(c)(f)"
      val expectedRegex = Concat(Word("ad"), Concat(Word("c"), Word("f")))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses union with lower precedence than anything else") {
      val testRegexString = "a|b*"
      val expectedRegex = Union(Word("a"), Star(Word("b")))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses 1(1)* correctly") {
      val testRegexString = "1(1)*"
      val expectedRegex = Concat(Word("1"), Star(Word("1")))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }

    it("parses kleene star with higher precedence than things in sequence") {
      val testRegexString = "jjjjj*"
    }

    it("parses a complicated expression") {
      val testRegexString = "(a*)|((k*i)|(ob))*|j*h"
      val middle = Star(Union(Concat(Star(Word("k")), Word("i")), Word("ob")))
      val expectedRegex = Union(Union(Star(Word("a")), middle), Concat(Star(Word("j")), Word("h")))
      assert(RegexStringParser.parse(testRegexString) == expectedRegex)
    }
  }
}
