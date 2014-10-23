package com.austinyoung.dfaregex

import scala.collection.mutable.{Map, SynchronizedMap, HashMap}

package object util {
  def uni(l: List[Char], t: List[Char]) = {
    var list = l
    var map = new HashMap[Char, Int]
    for (char <- l) {
      map.put(char, 1)
    }

    for (char <- t) {
      if (map.get(char) == None) {
        list = char :: list
        map.put(char, 1)
      }
    }
    list
  }
  type DFAMachine = Array[HashMap[Char, Int]]
  type NFAMachine = Array[HashMap[Char, List[Int]]]
}

class DFA(m: util.DFAMachine, a: List[Char], t: Array[Boolean]) {
  var machine = m
  var alph = a
  var accept = t

  def size = {
    this.machine.size
  }

  def eval(string: String) = {
    var current = 0
    var sList = string.toList
    while (sList != Nil) {
      current = this.machine(current).get(sList.head).get
      sList = sList.tail
    }
    this.accept(current)
  }

  def mapToState(size: Int) = {
    var i = 0
    var j = 0
    var k = 0
    var map = new HashMap[(Int, Int), Int]
    while (j < size) {
      while (i < this.size) {
        map.put((i, j), k)
        i = i + 1
        k = k + 1
      }
      i = 0
      j = j + 1
    }
    map
  }

  def mapToPair(size: Int) = {
    var i = 0
    var j = 0
    var k = 0
    var map = new Array[(Int, Int)](this.size * size)
    while (j < size) {
      while (i < this.machine.size) {
        map(k) = (i, j)
        i = i + 1
        k = k + 1
      }
      i = 0
      j = j + 1
    }
    map
  }

  def combine(dfa: DFA, op: (Boolean, Boolean) => Boolean): DFA = {
    var mapToState = this.mapToState(dfa.size)
    var mapToPair = this.mapToPair(dfa.size)

    var alphCombine = util.uni(this.alph, dfa.alph)
    var machineCombine = new util.DFAMachine(this.size * dfa.size)
    var acceptCombine = new Array[Boolean](this.size * dfa.size)
    for (i <- 0 to ((this.size * dfa.size) - 1)) {
      var map = new HashMap[Char, Int]
      for (char <- alphCombine) {
        var thisState = (this.machine(mapToPair(i)._1).get(char)).get
        var dfaState = (dfa.machine(mapToPair(i)._2).get(char)).get
        map.put(char, (mapToState.get((thisState, dfaState))).get)
      }
      machineCombine(i) = map
      acceptCombine(i) = (this.accept(mapToPair(i)._1) || dfa.accept(mapToPair(i)._2))
    }
    new DFA(machineCombine, alphCombine, acceptCombine)
  }

  def union(dfa: DFA) = {
    this.combine(dfa, (p: Boolean, q: Boolean) => (p || q))
  }

  def concat(dfa: DFA) = {}

  def star(dfa: DFA) = {}

  def toRegex = {}

  def toNFA = {}

}
