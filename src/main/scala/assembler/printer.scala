package assembler

import ast._

object printer {

  def error(msg: Any): Nothing = sys.error(msg.toString)

  def indent = "\t"

  def format(prg: Program): String = {
    val sb = new StringBuilder
    val it = prg.statements.iterator.buffered
    while (it.hasNext) {
      val s = it.next()
      sb ++= format(s)
      if (it.hasNext) (s, it.head) match {
        case (Comment(_, true), Comment(_, true))         => sb ++= "\n\t\t\t"
        case (Comment(_, _), Comment(_, true))            => sb += '\n'
        case (_, Comment(_, true))                        =>
        case (Label(s), _) if s.matches("B[0-9]+_[0-9]+") =>
        case (_, _)                                       => sb += '\n'
      }
    }
    sb.toString
  }

  def format(stmt: Statement): String = stmt match {
    case Label(name) => name + ":"
    case Directive(d, operands) if d == "file" || d == "loc" => indent + "." + d + "\t" + (operands map { format }).mkString(" ")
    case Directive(d, op1 :: operands) if d == "mask" || d == "fmask" => indent + "." + d + "\t" + formatHex(op1) + "," + (operands map { format }).mkString(",")
    case Directive(d, operands) => indent + "." + d + "\t" + (operands map { format }).mkString(",")
    case Instruction(i, operands) => indent + i + "\t" + (operands map { format }).mkString(", ")
    case LabelDefinition(name, v) => name + " = " + v
    case Comment(c, _) => "\t#" + c
    case EmptyLine => ""
  }

  def formatHex(op: Operand): String = op match {
    case IntegerConst(value, _) => f"0x$value%08x"
    case _ => format(op)
  }

  var octalQuotes = false
  def format(op: Operand): String = {
    def quoteChar(c: Char) = c match {
        case '\u0000'    => "\\0"
        case '\n'        => "\\n"
        case '\t'        => "\\t"
        case '\"'        => "\\\""
        case '\\'        => "\\\\"
        case c if c < 32 => f"\\${c.toInt}%03o"
        case c if c > 126 && octalQuotes => f"\\${c.toInt}%03o"
        case _           => c.toString
      }
    op match {
      case Register(name)                   => name
      case LabelRef(name)                   => name
      case IntegerConst(value, 10)          => value.toString
      case IntegerConst(value, 16)          => f"0x${value}%x"
      case IntegerConst(value, _)           => ??? // TODO: implement other bases?
      case FloatConst(value)                => value.toString
      case StringConst(value)               => "	\"" + (for (c <- value) yield quoteChar(c)).mkString + "\""
      case CharConst(value)                 => "	\'" + quoteChar(value) + "\'"
      case IndexedAddress(offset, base)     => format(offset) + "(" + format(base) + ")"
      case AssemblerFunction(name, operand) => "%" + name + "(" + format(operand) + ")"
      case ArithExpression(operation, a, b) => format(a) + " " + operation + " " + format(b)
      case Parenthesis(expr)                => "(" + format(expr) + ")"
    }
  }
}
