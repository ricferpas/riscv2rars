package assembler

import ast._
import linker._

object assembler {

  def error(msg: Any) = sys.error(msg.toString)

  def assemble(file: String) = {
    val prog = Program.fromFile(file)
    val result = new Linkable
    var curSection = result.addSection()
    for (stmt ← prog.statements) stmt match {
      case Label(name: String) ⇒
        result.addLabel(name, SectionRef(curSection, curSection.position))

      case Directive("globl", Seq(ast.LabelRef(name))) ⇒
        result.addGlobal(name)

      case Directive("data", Seq()) ⇒
        curSection = result.addSection(SectionType.Data)
      case Directive("data", Seq(IntegerConst(addr))) ⇒
        curSection = result.addSection(SectionType.Data, Some(addr))
      case Directive("data", _) ⇒
        error(stmt)

      case Directive("text", Seq()) ⇒
        curSection = result.addSection(SectionType.Text)
      case Directive("text", Seq(IntegerConst(addr))) ⇒
        curSection = result.addSection(SectionType.Text, Some(addr))
      case Directive("text", _) ⇒
        error(stmt)

      case Directive("word", words) ⇒
        words map { w ⇒ curSection.appendWord(w.toWord) }

      case Directive("space", Seq(size)) ⇒
        curSection.appendSpace(size.toOffset)

      case Instruction("addi", Seq(dest: Register, srcA: Register, o)) ⇒
        curSection.appendIInst(0x08, dest.toRegId, srcA.toRegId, o.toInt)

      case Instruction("sw", Seq(src: Register, IndexedAddress(offset, base))) ⇒
        curSection.appendIInst(0x2b, base.toRegId, src.toRegId, offset.toInt)
      case Instruction("lw", Seq(dest: Register, IndexedAddress(offset, base))) ⇒
        curSection.appendIInst(0x2c, base.toRegId, dest.toRegId, offset.toInt)

      case Instruction("sw", Seq(src: Register, ast.LabelRef(label))) ⇒ {
        curSection.addRelocation(Relocation(linker.LabelRef(label, 0)))
        curSection.appendIInst(0x0f, 0, 1, 0)
        curSection.appendIInst(0x2b, 1, src.toRegId, 0)
      }
      case Instruction("lw", Seq(dest: Register, ast.LabelRef(label))) ⇒ {
        curSection.addRelocation(Relocation(linker.LabelRef(label, 0)))
        curSection.appendIInst(0x0f, 0, 1, 0)
        curSection.appendIInst(0x2c, 1, dest.toRegId, 0)
      }
    }
    result
  }

  implicit class operandOps(o: Operand) {
    def toInteger: Long = o match {
      case IntegerConst(v) ⇒ v
      case _               ⇒ error("no es un número")
    }
    def toInt: Int = toInteger.toInt
    def toOffset: Int = toInteger.toInt
    def toWord: Word = toInteger.toInt

    val regNames = Map(
      "zero" → 0,
      "at" → 1,
      "v0" → 2,
      "v1" → 3,
      "a0" → 4,
      "a1" → 5,
      "a2" → 6,
      "a3" → 7,
      "t0" → 8,
      "t1" → 9,
      "t2" → 10,
      "t3" → 11,
      "t4" → 12,
      "t5" → 13,
      "t6" → 14,
      "t7" → 15,
      "s0" → 16,
      "s1" → 17,
      "s2" → 18,
      "s3" → 19,
      "s4" → 20,
      "s5" → 21,
      "s6" → 22,
      "s7" → 23,
      "t8" → 24,
      "t9" → 25,
      "k0" → 26,
      "k1" → 27,
      "gp" → 28,
      "sp" → 29,
      "fp" → 30,
      "ra" → 31) ++ (for (i ← 0 to 31) yield s"r$i" → i) ++ (for (i ← 0 to 31) yield s"$i" → i)

    def toRegId: Int = o match {
      case Register(v) ⇒ regNames.getOrElse(v, error("registro erróneo " + v))
      case _           ⇒ error("no es un registro")
    }
  }
}

