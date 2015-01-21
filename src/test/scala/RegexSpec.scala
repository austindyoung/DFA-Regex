package com.austinyoung.dfaregex

import org.scalatest.FunSpec

class RegexSpec extends FunSpec {

  describe("Regex") {

    it("leaf node contructed") {
      val charSeq = "I'mNotAWord"
      val numSeq = List(1, 0)
      val charWord = new Word[Char](charSeq)
      val numWord = new Word[Int](numSeq)
      assert(charWord.content == Seq(charSeq: _*))
      assert(numWord.content == numSeq)
    }

    it("supports second-order node construction") {
      val leftCharSeq = "left"
      val rightCharSeq = "right"
      val leftIntSeq = List(0, 0)
      val rightIntSeq = List(0, 1)

      val leftCharWord = new Word(leftCharSeq)
      val rightCharWord = new Word(rightCharSeq)
      val leftIntWord = new Word(leftIntSeq)
      val rightIntWord = new Word(rightIntSeq)

      val starChar = new Star[Char](leftCharWord)
      val starInt = new Star[Int](leftIntWord)
      assert(starChar.content == leftCharWord)
      assert(starInt.content == leftIntWord)

      val unionChar = new Union[Char](leftCharWord, rightCharWord)
      val unionInt = new Union[Int](leftIntWord, rightIntWord)
      assert(unionChar.left == leftCharWord)
      assert(unionChar.right == rightCharWord)
      assert(unionInt.left == leftIntWord)
      assert(unionInt.right == rightIntWord)

      val concatChar = new Concat[Char](leftCharWord, rightCharWord)
      val concatInt = new Concat[Int](leftIntWord, rightIntWord)
      assert(concatChar.left == leftCharWord)
      assert(concatChar.right == rightCharWord)
      assert(concatInt.left == leftIntWord)
      assert(concatInt.right == rightIntWord)
    }

    it("supports nested equality") {
      assert(new Union(Word("test"), Word("fun")) == new Union(Word("test"), Word("fun")))
      assert(new Union(Star(Word("test")), Concat(Word("fun"), Word("bar"))) ==
               new Union(Star(Word("test")), Concat(Word("fun"), Word("bar"))))
    }

    it("supports conversion to DFA") {
      val evenZerosRegex = new Concat[Int](new Star(new Word(List(1))), new Concat(new Star(new Concat(new Word(List(0)), new Concat(new Star(new Word(List(1))), new Word(List(0))))), new Star(new Word(List(1)))))

      val evenZerosDFAFromRegex = evenZerosRegex.toDFA
      assert(evenZerosDFAFromRegex.evaluate(Nil))
      assert(!evenZerosDFAFromRegex.evaluate(List(0)))
      assert(evenZerosDFAFromRegex.evaluate(List(0, 0)))
      assert(evenZerosDFAFromRegex.evaluate(List(0, 1, 0)))
      assert(!evenZerosDFAFromRegex.evaluate(List(1, 0, 1)))

      assert(evenZerosRegex.left == new Star(new Word(List(1))))
    }

    it("converts unions to DFA properly") {
      Union(Atom('a'), Atom('b')).toDFA
    }
  }
}
