package assembler

import ast._
import scala.util.parsing.combinator._
import scala.util.matching.Regex

object parser extends RegexParsers {
  def program: Parser[Program] = repsep(line, lineBreak) ^^ { l ⇒ Program(l.reduce(_ ++ _)) }
  def lineBreak = """\n|\r|\r\n""".r
  override val whiteSpace = """[ \t\x0B\f]+""".r

  def identifier: Parser[String] =
    """(\p{javaJavaIdentifierStart}|[.@])(\p{javaJavaIdentifierPart}|[.@])*""".r

  def number =
    (integer | real | character)

  val integer =
    """0x[\da-zA-Z]+""".r ^^ { s ⇒ IntegerConst(java.lang.Long.parseLong(s.drop(2), 16)) } |
      """-?\d+""".r ^^ { s ⇒ IntegerConst(s.toLong) }

  val real =
    """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ { s ⇒ FloatConst(s.toDouble) }

  protected def regexNoWs(r: Regex) = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val start = in.offset
      (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) ⇒
          Success(source.subSequence(start, start + matched.end).toString, in.drop(matched.end))
        case None ⇒
          val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
          Failure("string matching regex `" + r + "' expected but " + found + " found", in)
      }
    }
  }

  def character_except(c: Char) = {
    regexNoWs("""\\[0-7]+""".r) ^^ { s ⇒ Integer.parseInt(s.substring(1), 8).toChar }
  } | {
    regexNoWs("""\\.""".r) ^^ { s ⇒
      s.charAt(1) match {
        case 'n' ⇒ '\n'
        case _   ⇒ sys.error("carácter " + s)
      }
    }
  } | {
    regexNoWs(s"[^$c]".r) ^^ { _.charAt(0) }
  }

  val character =
    "\'" ~> character_except('\'') <~ "\'" ^^ { CharConst(_) }

  val string =
    "\"" ~> rep(character_except('\"')) <~ "\"" ^^ { s ⇒ StringConst(s.mkString) }

  def register =
    "$" ~> (integer ^^ { _.value.toString } | identifier) ^^ { Register(_) }

  def line: Parser[Seq[Statement]] = {
    (identifier <~ "=") ~ ".*".r ^^ { case l ~ v ⇒ Seq(LabelDefinition(l, v.mkString)) }
  } | {
    opt(label) ~ opt(statement) ~ opt(comment) ^^ {
      case olabel ~ ostmt ~ None ⇒
        (olabel map { Label(_) }).toSeq ++ ostmt.toSeq
      case olabel ~ Some(stmt) ~ Some(c) ⇒
        (olabel map { Label(_) }).toSeq :+ stmt :+ Comment(c, true)
      case Some(label) ~ None ~ Some(c) ⇒
        Seq(Label(label), Comment(c, true))
      case None ~ None ~ Some(c) ⇒
        Seq(Comment(c))
    }
  }

  val label = identifier <~ ":"

  val comment = """#.*""".r ^^ { s ⇒ s.substring(1) }

  def statement: Parser[Statement] =
    directive | instruction

  def directive =
    "." ~ identifier ~ repsep(operand, opt(",")) ^^ { case _ ~ name ~ operands ⇒ Directive(name, operands) }

  def instruction =
    (identifier ~ repsep(operand, ",")) ^^ { case opcode ~ operands ⇒ Instruction(opcode, operands) }

  def operand = addressRef | expression

  def value: Parser[Operand] =
    number | register | labelRef | string | assemblerFunction

  def labelRef = identifier ^^ { LabelRef(_) }

  def addressRef = expression ~ "(" ~ expression ~ ")" ^^ { case offset ~ _ ~ register ~ _ ⇒ IndexedAddress(offset, register) }

  def assemblerFunction = "%" ~> identifier ~ ("(" ~> expression <~ ")") ^^ { case i ~ e ⇒ AssemblerFunction(i, e) }

  def expression: Parser[Operand] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case ta ~ terms ⇒ terms.foldLeft(ta) { case (acc, s ~ tb) ⇒ ArithExpression(s, acc, tb) }
  }
  def term = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case fa ~ factors ⇒ factors.foldLeft(fa) { case (acc, s ~ fb) ⇒ ArithExpression(s, acc, fb) }
  }
  def factor = value | ("(" ~> (expression ^^ { e ⇒ Parenthesis(e) }) <~ ")")
}
