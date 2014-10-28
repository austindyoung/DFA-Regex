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
   it("language node constructed") {
   }
  }
}
