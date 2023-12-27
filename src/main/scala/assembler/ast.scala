package assembler

import scala.annotation.tailrec

object ast {
  case class Program(statements: Seq[Statement]) {
    def findLabel(label: String): LazyList[Statement] = {
      @tailrec
      def loop(it: LazyList[Statement]): LazyList[Statement] = {
        if (it.isEmpty) it
        else it.head match {
          case Label(l) if l == label => it
          case _                      => loop(it.tail)
        }
      }
      loop(statements.to(LazyList))
    }
  }
  object Program {
    def fromReader(r: java.io.Reader) = {
      parser.parseAll(parser.program, r) match {
        case parser.Success(prog, _) => prog
        case x                       => sys.error(x.toString)
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
      case Instruction(op, _) if Set("beq", "bne", "j", "jr", "jal", "jalr", "bltz", "beqz", "bc1f", "bgez", "bgezal", "bgtz", "blez", "bltzal", "b", "beqz", "bge", "bgeu", "bgt", "bgtu", "ble", "bleu", "blt", "bltu", "bnez") contains op => true
      case _ => false
    }
    def isNop = this match {
      case Instruction("nop", _) => true
      case _                     => false
    }
    def outputOperands: Seq[Operand] = this match {
      case Instruction(inst, _) if Set("jal", "jalr")(inst) => Seq(Register("v0"), Register("v1"), Register("ra"))
      case Instruction(inst, _) if Set("j", "b", "jr", "beq", "beqz", "bne", "bnez", "bltz", "ble", "blez", "bge", "bgez", "blt")(inst) => Seq.empty
      case Instruction(inst, Seq(_, o)) if Set("sw", "sh", "sb", "swc1", "mtc1", "sdc1")(inst) => Seq(o)
      case Instruction(inst, Seq(o, _)) if Set("lw", "lwc1", "ldc1", "lh", "lhu", "lb", "lbu", "move", "la", "li", "lui", "not", "cvt.s.w", "cvt.w.s", "trunc.w.s", "mfc1")(inst) => Seq(o)
      case Instruction(inst, Seq(o, _, _)) if Set("add", "add.s", "addiu", "addi", "sub", "andi", "sll", "sra", "subu", "subi", "slt", "slti", "sltu", "addu", "mul", "mul.s", "div", "div.s", "movn", "movz", "xor", "or", "and", "ori")(inst) => Seq(o)
      case Instruction("nop", _) => Seq.empty
      case Instruction("syscall", _) => Seq(Register("v0"), Register("a0"), Register("a1")) // FIXME
      case Directive(_, _) => Seq.empty
      case Label(_) => Seq.empty
      case LabelDefinition(_, _) => Seq.empty
      case Comment(_, _) => Seq.empty
      case EmptyLine => Seq.empty
      case _ => ??? // Not implemented yet
    }
    def inputOperands: Seq[Operand] = this match {
      case Instruction(inst, ops) if Set("jal", "jalr")(inst) => ops ++ Seq("a0", "a1", "a2", "a3", "f12", "f13", "f14", "f15").map(Register(_))
      case Instruction(inst, ops) if Set("j", "b", "jr", "beq", "beqz", "bne", "bnez", "bltz", "ble", "blez", "bge", "bgez", "blt")(inst) => ops
      case Instruction(inst, Seq(i, IndexedAddress(offset, base))) if Set("sw", "sh", "sb", "swc1")(inst) => Seq(i, offset, base)
      case Instruction(inst, Seq(_, IndexedAddress(offset, base))) if Set("lw", "lh", "lb", "lhu", "lbu")(inst) => Seq(offset, base)
      case Instruction(inst, Seq(i, _)) if Set("sw", "sh", "sb", "swc1", "mtc1", "sdc1")(inst) => Seq(i)
      case Instruction(inst, Seq(_, i)) if Set("lw", "lwc1", "ldc1", "lh", "lhu", "lb", "lbu", "move", "la", "li", "lui", "not", "cvt.s.w", "cvt.w.s", "trunc.w.s", "mfc1")(inst) => Seq(i)
      case Instruction(inst, Seq(_, a, b)) if Set("add", "add.s", "addiu", "addi", "sub", "andi", "sll", "sra", "subu", "subi", "slt", "slti", "sltu", "addu", "mul", "mul.s", "div", "div.s", "movn", "movz", "xor", "or", "and", "ori")(inst) => Seq(a, b)
      case Instruction("nop", _) => Seq.empty
      case Instruction("syscall", _) => Seq(Register("v0"), Register("a0"), Register("a1"), Register("a2"), Register("a3"))
      case Directive(_, _) => Seq.empty
      case Label(_) => Seq.empty
      case LabelDefinition(_, _) => Seq.empty
      case Comment(_, _) => Seq.empty
      case EmptyLine => Seq.empty
      case _ => ??? // Not implemented yet
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
    def isCalleeSaved = Register.calleeSavedNames contains name
    def isProcInput = Register.procInputNames contains name
    def isProcOutput = Set("v0", "v1", "2", "3", "f0", "f1", "f2", "f3")(name)
  }
  object Register {
    val regNames = Set(
      "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", "x31",
      "zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t6", "t6",
      "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
      "ft0", "ft1", "ft2", "ft3", "ft4", "ft5", "ft6", "ft7", "fs0", "fs1", "fa0", "fa1", "fa2", "fa3", "fa4", "fa5", "fa6", "fa7", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7", "fs8", "fs9", "fs10", "fs11", "ft8", "ft9", "ft10", "ft11"
    )
    val calleeSavedNames = Set("sp", "x2", "s0", "x8") // TODO
    val procInputNames = Set("a0", "a1", "a2", "a3") // TODO
    val procOutputNames = procInputNames // TODO
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
