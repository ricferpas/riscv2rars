package assembler

import ast._

object printer {

  def error(msg: Any) = sys.error(msg.toString)

  def indent = "\t"

  def format(prg: Program): String = {
    val sb = new StringBuilder
    val it = prg.statements.iterator.buffered
    while (it.hasNext) {
      val s = it.next
      sb ++= format(s)
      if (it.hasNext) (s, it.head) match {
        case (_, Comment(_, true))                        ⇒
        case (Label(s), _) if s.matches("B[0-9]+_[0-9]+") ⇒
        case (_, _)                                       ⇒ sb += '\n'
      }
    }
    sb.toString
  }

  def format(stmt: Statement): String = stmt match {
    case Label(name) ⇒ name + ":"
    case Directive(d, operands) if d == "file" || d == "loc" ⇒ indent + "." + d + "\t" + (operands map { format(_) }).mkString(" ")
    case Directive(d, op1 :: operands) if d == "mask" || d == "fmask" ⇒ indent + "." + d + "\t" + formatHex(op1) + "," + (operands map { format(_) }).mkString(",")
    case Directive(d, operands) ⇒ indent + "." + d + "\t" + (operands map { format(_) }).mkString(",")
    case Instruction(i, operands) ⇒ indent + i + "\t" + (operands map { format(_) }).mkString(", ")
    case LabelDefinition(name, v) ⇒ name + " = " + v
    case Comment(c, _) ⇒ "\t#" + c
    case EmptyLine ⇒ ""
  }

  def formatHex(op: Operand): String = op match {
    case IntegerConst(value) ⇒ f"0x$value%08x"
    case _                   ⇒ format(op)
  }

  var octalQuotes = false
  def format(op: Operand): String = {
    def quoteChar(c: Char) =
      if (octalQuotes) c match {
        case c if (c > 126) ⇒ f"\\${c}%03o"
        case '\n'           ⇒ "\\n"
        case c if (c < 32)  ⇒ f"\\${c}%03o"
        case _              ⇒ c.toString
      }
      else c match {
        case '\0'          ⇒ "\\0"
        case '\n'          ⇒ "\\n"
        case '\t'          ⇒ "\\t"
        case c if (c < 32) ⇒ f"\\${c}%03o"
        case _             ⇒ c.toString
      }
    op match {
      case Register(name)                   ⇒ "$" + name
      case LabelRef(name)                   ⇒ name
      case IntegerConst(value)              ⇒ value.toString
      case FloatConst(value)                ⇒ value.toString
      case StringConst(value)               ⇒ "	\"" + (for (c ← value) yield quoteChar(c)).mkString + "\""
      case CharConst(value)                 ⇒ "	\'" + quoteChar(value) + "\'"
      case IndexedAddress(offset, base)     ⇒ format(offset) + "(" + format(base) + ")"
      case AssemblerFunction(name, operand) ⇒ "%" + name + "(" + format(operand) + ")"
      case ArithExpression(operation, a, b) ⇒ format(a) + " " + operation + " " + format(b)
      case Parenthesis(expr)                ⇒ "(" + format(expr) + ")"
    }
  }
}
