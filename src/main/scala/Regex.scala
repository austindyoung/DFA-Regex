abstract class Regex[T]

case class Word[T](content: Seq[T]) extends Regex[T]

case class Star[T](content: Regex[T]) extends Regex[T]

case class Union[T](left: Regex[T], right: Regex[T]) extends Regex[T]

case class Concat[T](left: Regex[T], right: Regex[T]) extends Regex[T]

abstract class Token[+T]

case class Epsilon extends Token[+T]

case class Letter[+T](letter: +T) extends Token[+T]

case class Operator(op: Char) extends Token

class Parser[+T](s: Seq[Token[+T]], n: Int,  e: Int, f: Int) {
  var stream = s
  var next = n
  var end = e
  var failed = f

  def CFGBuilder(last: Int, save: Int, rules: List[(Int => Option[Regex[T]])]): Option[Regex[T]] = {
    rules match {
      case List(f) => f(last)
      case _   => {
        val result = rules.head(last)
        if (result == None) {
        failed = max(next, failed)
        next = save
        CFGBuilder(last, save, rules.tail)
      }
      else result
    }
  }
  }

  val Es: List[(Int => Option[Regex[T]])] = List(T, parseT_OR_E)
  val Ts: List[(Int => Option[Regex[T]])] = List(LETTER, parseLETTER_STAR, parseLETTER_T, parseLETTER_STAR_T, parseOPEN_E_CLOSE, parseOPEN_E_CLOSE_STAR, parseOPEN_E_CLOSE_T, parseOPEN_E_CLOSE_STAR_T)

  def max(n: Int, m: Int) = if (n <= m) m else n

  def parse = {
    E(end).getOrElse("syntax error at " ++ failed.toString())
  }

  def E(last: Int): Option[Regex[T]] = {

    if (last < next || last >= stream.size) None
    else {
      val save = next
      CFGBuilder(last, save, Es)
    }
  }

  def T(last: Int): Option[Regex[T]] = {
    if (last < next || last >= stream.size) None
    else {
      val save = next
      CFGBuilder(last, save, Ts)
    }
  }
   
  def opToken(op: Char, place: Int) = {
    next = next + 1
    if (next > end || end >= stream.size) false
    else {
      if (next == place) {
        stream(next) match {
          case Operator(token) => {
            if (token == op) true
            else false
          }
          case _ => false
        }
      }
      else false
    }
  }

  def OR = opToken('|', )

  def OPEN = opToken('(')

  def CLOSE = opToken(')')

  def STAR(last: Int) = {
    next = next + 1
    if (next > last || last >= stream.size) false
    else {
    if (next == last) {
      val eitherToken = stream(next)
        eitherToken match {
          case Operator(token) => {
            if (x == '*') true
            else false
          }
          case _ => false
        }
    }
    else false
    }
  }

  def LETTER(last: Int) = {
    next = next + 1
    if (next > last || last >= stream.size) None
    else {
    if (next == last) {
      stream(next) match {
        case Letter(letter) => Some(new Word(List(letter)))
        case Right(x) => None
      }
    }
    else None
    }
  }

/** LETTER* */
  def parseLETTER_STAR(last: Int) = {
    val letter = LETTER(last - 1)
    if (letter != None) {
      if (STAR(last)) Some(new Star(letter.get))
      else None
    }
    else None
  }

/** LETTER T */
  def parseLETTER_T(last: Int) = {
    val left = LETTER(next + 1)
    if (left != None) {
      val right = T(last)
      if (right != None) Some(new Concat[T](left.get, right.get))
      else None
    }
    else None
  }

/** LETTER* T */
  def parseLETTER_STAR_T(last: Int) = {
    val letter = LETTER(next + 1)
    if (letter != None) {
      if (STAR(next + 1)) {
        val t = T(last)
        if (t != None) Some(new Concat[T](new Star[T](letter.get), t.get))
        else None
      }
      else None
    }
    else None
  }



/** T | E */
   def parseT_OR_E(last: Int) = {
    val left = T(failed - 1)
    if (left != None) {
      if (OR) {
        val right = E(last)
        if (right != None) Some(new Union(left.get, right.get))
        else None
      }
      else None
    }
    else None
  }

/** (E) */
  def parseOPEN_E_CLOSE(last: Int) = {
    if (OPEN) {
      val inside = E(last - 1)
      if (inside != None) {
        if (CLOSE) inside
        else None
      }
      else None
    }
    else None
  }

/** (E)* */
  def parseOPEN_E_CLOSE_STAR(last: Int) = {
    if (OPEN) {
      val inside = E(last - 2)
      if (inside != None) {
        if (CLOSE) {
          if (STAR(next + 1)) Some(new Star[T](inside.get))
          else None
        }
        else None
      }
      else None
    }
    else None
  }

/** (E) T */
  def parseOPEN_E_CLOSE_T(last: Int) = {
    val left = parseOPEN_E_CLOSE(failed)
    if (left != None) {
      val right = T(last)
      if (right != None) Some(new Concat[T](left.get, right.get))
      else None
    }
    else None
  }

  
/** (E)* T */
  def parseOPEN_E_CLOSE_STAR_T(last: Int) = {
    val left = parseOPEN_E_CLOSE(failed - 1)
    if (left != None) {
      if (STAR(failed)) {
        val t = T(last)
        if (t != None) Some(new Concat[T](new Star[T](left.get), t.get))
        else None
      }
      else None
    }
    else None
  }
}

class Parse[T] {
  def parse(exp: Seq[Either[T,Char]]) = (new Parser[T](exp, -1, exp.size - 1, 0)).parse
}

class ParseChar {
  def parseChar(exp: String) = {
    def mapper(token: Char): Either[Char, Char] = if (token != '(' && token != ')' && token != '*' && token != '|') Left(token) else Right(token)
    new Parse[Char].parse(exp.toSeq.map(mapper))
  }
}
