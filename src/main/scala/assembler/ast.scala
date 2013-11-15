package assembler

object ast {
  case class Program(statements: Seq[Statement])
  object Program {

    def fromReader(r: java.io.Reader) = {
      parser.parseAll(parser.program, r) match {
        case parser.Success(prog, _) ⇒ prog
        case x                       ⇒ { sys.error(x.toString()) }
      }
    }

    def fromFile(file: String) = {
      val f = new java.io.FileReader(file)
      try fromReader(f)
      finally f.close()
    }
  }

  sealed abstract class Statement {
    def isJump = this match {
      case Instruction(op, _) if Set("beq", "bne", "j", "jal", "jr") contains op ⇒ true
      case _ ⇒ false
    }
    def isNop = this match {
      case Instruction("nop", _) ⇒ true
      case _                     ⇒ false
    }
  }
  case class Label(name: String) extends Statement
  case class LabelDefinition(name: String, value: String) extends Statement
  case class Directive(directive: String, operands: Seq[Operand]) extends Statement
  case class Instruction(opcode: String, operands: Seq[Operand]) extends Statement
  case class Comment(comment: String, attached: Boolean = false) extends Statement
  case object EmptyLine extends Statement

  sealed abstract class Operand
  case class Register(name: String) extends Operand
  case class LabelRef(name: String) extends Operand
  case class IndexedAddress(offset: Operand, base: Operand) extends Operand
  case class AssemblerFunction(name: String, operand: Operand) extends Operand

  sealed abstract class Literal extends Operand
  case class IntegerConst(value: Long) extends Literal
  case class FloatConst(value: Double) extends Literal
  case class StringConst(value: String) extends Literal
  case class CharConst(value: Char) extends Literal

  sealed abstract class Expression extends Operand
  case class ArithExpression(operation: String, a: Operand, b: Operand) extends Expression
  case class Parenthesis(expressions: Operand) extends Expression
}
