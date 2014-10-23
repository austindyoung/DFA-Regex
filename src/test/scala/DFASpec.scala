package com.austinyoung.dfaregex

import org.scalatest.FunSpec
import scala.collection.mutable.{Map, SynchronizedMap, HashMap}

class DFASpec extends FunSpec {
  describe("Same number of zeros and ones DFA") {
    it("Check that the same number of zeros and ones are given.") {
      // TODO(@IvanMalison) What is this test doing
      var zero = new HashMap[Char, Int]
      var one = new HashMap[Char, Int]
      zero.put('0', 1)
      zero.put('1', 0)
      one.put('0', 0)
      one.put('1', 1)
      var accept = new Array[Boolean](2)
      accept(0) = true
      accept(1) = false
      var machine = new Array[HashMap[Char, Int]](2)
      machine(0) = zero
      machine(1) = one
      var zero1 = new HashMap[Char, Int]
      var one1 = new HashMap[Char, Int]
      zero1.put('0', 0)
      zero1.put('1', 1)
      one1.put('0', 1)
      one1.put('1', 0)
      var machine1 = new Array[HashMap[Char, Int]](2)
      machine1(0) = zero1
      machine1(1) = one1
      var alph = List('0', '1')
      // TODO(@IvanMalison) Add assertions.
    }
  }
}

