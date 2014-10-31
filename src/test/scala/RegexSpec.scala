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

   it("Complex node constructed") {
     val leftCharWord = "left"
     val rightCharWord = "right"
     val leftIntWord = List(0, 0)
     val rightIntWord = List(0, 1)
     
     val starChar = new Star[Char](leftCharWord)
     val starInt = new Star[Int](leftIntWord)
     assert(starChar.left == leftCharWord)
     assert(starChar.right == rightCharWord)
     assert(starInt.left == leftIntWord)
     assert(starInt.right == rightIntWord)


     val unionChar = new Union[Char](leftCharWord, rightCharWord)
     val unionInt = new Union[Char](leftIntWord, rightIntWord)
     assert(unionChar.left == leftCharWord)
     assert(unionChar.right == rightCharWord)
     assert(unionInt.left == leftIntWord)
     assert(unionInt.right == rightIntWord)

     val concatChar = new Concat[Char](leftCharWord, rightCharWord)
     val concatInt = new Concat[Int](leftIntword, rightIntWord)
     assert(concatChar.left == leftCharWord)
     assert(concatChar.right == rightCharWord)
     assert(concatInt.left == leftIntWord)
     assert(concatInt.right == rightIntWord)
   }
  }
}
