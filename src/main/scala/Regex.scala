class Regex {

  case class Lang(dfa: DFA) extends Regex

  case class Set(strings: List[String]) extends Regex

  case class *(regex: Regex) extends Regex
  
  case class U(regex1: Regex, regex2: Regex) extends Regex

  case class o(regex1: Regex, regex2: Regex) extends Regex

  def toDFA: DFA = {
    this match {
      case *(regex) => regex.toDFA.star
      case U(regex1, regex2) => regex1.toDFA union regex2.toDFA
      case o(regex1, regex2) => regex1.toDFA concat regex2.toDFA 
      case Set(strings)=> {
        strings match {
          case List(char) => {
            var newMachine = new Array[(Boolean, HashMap[Char, Int])](string.length)
            var newAlph = Nil
            var charOccur = new HashMap[Char, Int]
            var sList = string.toList
            var list = sList
            while (sList != Nil) {
              if (charOccur(sList.head) == None) {
                charOccur.put(sList.head, 1)
                newAlph = sList.head :: newAlph
              }
              sList = sList.tail
            }
            for (i <- 0 to string.length - 1) {
              for (char <- newAlph) {
                if (char == list.head) machine(i)._2.put(char, i + 1)
                else machine(i)._2.put(char, string.length + 2)
              }
            }
            for (char <- newAlph) {
              machine(i)._2.put(char, i + 1)
            }
            for (char <- newAlph) {
              machine(i + 1)._2.put(char, i + 1)
            }

            new DFA(newMachine, newAlph)
          }
          case string :: strings => (new Set(List(string))).toDFA.union((new Set(strings)).toDFA)
          case Nil =>               new DFA(Array(), Nil)
        }
      }
      case Lang(dfa) => dfa
    }
  }

  def * =
    new *(this)

  def U(regex: Regex) =
    new U(this, regex)

  def o(regex: Regex) =
    new o(this, regex)
}
