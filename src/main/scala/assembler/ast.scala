package assembler

object ast {
  case class Program(statements: Seq[Statement]) {
    def findLabel(label: String): Stream[Statement] = {
      def loop(it: Stream[Statement]): Stream[Statement] = it match {
        case s #:: rest ⇒ s match {
          case Label(l) if l == label ⇒ it
          case _                      ⇒ loop(rest)
        }
        case Stream.Empty ⇒ it
      }
      loop(statements.toStream)
    }
  }
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
      case Instruction(op, _) if Set("beq", "bne", "j", "jr", "jal", "jalr", "bltz", "beqz", "bc1f", "bgez", "bgezal", "bgtz", "blez", "bltzal", "b", "beqz", "bge", "bgeu", "bgt", "bgtu", "ble", "bleu", "blt", "bltu", "bnez") contains op ⇒ true
      case _ ⇒ false
    }
    def isNop = this match {
      case Instruction("nop", _) ⇒ true
      case _                     ⇒ false
    }
    def outputOperands: Seq[Operand] = this match {
      case Instruction(inst, _) if Set("jal", "jalr")(inst) ⇒ Seq(Register("v0"), Register("v1"), Register("ra"))
      case Instruction(inst, _) if Set("j", "b", "jr", "beq", "beqz", "bne", "bnez", "bltz", "ble", "blez", "bge", "bgez", "blt")(inst) ⇒ Seq.empty
      case Instruction(inst, Seq(_, o)) if Set("sw", "sh", "sb", "swc1", "mtc1")(inst) ⇒ Seq(o)
      case Instruction(inst, Seq(o, _)) if Set("lw", "lwc1", "lh", "lhu", "lb", "lbu", "move", "la", "li", "lui", "not", "cvt.s.w", "cvt.w.s", "mfc1")(inst) ⇒ Seq(o)
      case Instruction(inst, Seq(o, _, _)) if Set("add", "add.s", "addiu", "addi", "sub", "andi", "sll", "sra", "subu", "subi", "slt", "slti", "sltu", "addu", "mul", "mul.s", "div", "div.s")(inst) ⇒ Seq(o)
      case Instruction("nop", _) ⇒ Seq.empty
      case Instruction("syscall", _) ⇒ Seq(Register("v0"), Register("a0"), Register("a1")) // FIXME
      case Directive(_, _) ⇒ Seq.empty
      case Label(_) ⇒ Seq.empty
      case LabelDefinition(_, _) ⇒ Seq.empty
      case Comment(_, _) ⇒ Seq.empty
      case EmptyLine ⇒ Seq.empty
    }
    def inputOperands: Seq[Operand] = this match {
      case Instruction(inst, ops) if Set("jal", "jalr")(inst) ⇒ ops ++ (Seq("a0", "a1", "a2", "a3", "f12", "f13", "f14", "f15") map { Register(_) })
      case Instruction(inst, ops) if Set("j", "b", "jr", "beq", "beqz", "bne", "bnez", "bltz", "ble", "blez", "bge", "bgez", "blt")(inst) ⇒ ops
      case Instruction(inst, Seq(i, IndexedAddress(offset, base))) if Set("sw", "sh", "sb", "swc1")(inst) ⇒ Seq(i, offset, base)
      case Instruction(inst, Seq(_, IndexedAddress(offset, base))) if Set("lw", "lh", "lb", "lhu", "lbu")(inst) ⇒ Seq(offset, base)
      case Instruction(inst, Seq(i, _)) if Set("sw", "sh", "sb", "swc1", "mtc1")(inst) ⇒ Seq(i)
      case Instruction(inst, Seq(_, i)) if Set("lw", "lwc1", "lh", "lhu", "lb", "lbu", "move", "la", "li", "lui", "not", "cvt.s.w", "cvt.w.s", "mfc1")(inst) ⇒ Seq(i)
      case Instruction(inst, Seq(_, a, b)) if Set("add", "add.s", "addiu", "addi", "sub", "andi", "sll", "sra", "subu", "subi", "slt", "slti", "sltu", "addu", "mul", "mul.s", "div", "div.s")(inst) ⇒ Seq(a, b)
      case Instruction("nop", _) ⇒ Seq.empty
      case Instruction("syscall", _) ⇒ Seq(Register("v0"), Register("a0"), Register("a1"), Register("a2"), Register("a3"))
      case Directive(_, _) ⇒ Seq.empty
      case Label(_) ⇒ Seq.empty
      case LabelDefinition(_, _) ⇒ Seq.empty
      case Comment(_, _) ⇒ Seq.empty
      case EmptyLine ⇒ Seq.empty
    }
  }
  case class Label(name: String) extends Statement
  case class LabelDefinition(name: String, value: String) extends Statement
  case class Directive(directive: String, operands: Seq[Operand]) extends Statement
  case class Instruction(opcode: String, operands: Seq[Operand]) extends Statement
  case class Comment(comment: String, attached: Boolean = false) extends Statement
  case object EmptyLine extends Statement

  sealed abstract class Operand
  case class Register(name: String) extends Operand {
    def isCalleeSaved = Set("s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "16", "17", "18", "19", "20", "21", "22", "23", "gp", "28", "sp", "29", "fp", "30", "ra", "31")(name)
    def isProcInput = Set("a0", "a1", "a2", "a3", "4", "5", "6", "7", "f12", "f13", "f14", "f15")(name)
    def isProcOutput = Set("v0", "v1", "2", "3", "f0", "f1", "f2", "f3")(name)
  }
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
