package mips2mars

import scala.collection.mutable.Buffer
import assembler._
import ast._

object transforms {
  type Transform = Program ⇒ Program

  def removeDelaySlot(prg: Program) = {
    val it = prg.statements.iterator
    val ret = Buffer[Statement]()
    while (it.hasNext) {
      val s = it.next
      if (s.isJump) {
        val t = it.next
        if (!t.isNop) ret += t
      }
      ret += s
    }
    Program(ret)
  }

  def removeDirectives(prg: Program, directives: Set[String] = Set("set", "mask", "fmask", "frame", "cfi_sections", "cfi_startproc", "cfi_endproc", "cfi_def_cfa_offset", "cfi_offset", "ent", "type", "end", "previous", "size", "file", "loc", "local")) =
    Program(prg.statements filter { s ⇒ s match { case Directive(d, _) if (directives contains d) ⇒ false case _ ⇒ true } })

  def removeComments(prg: Program) =
    Program(prg.statements filter { s ⇒ s match { case Comment(_, _) ⇒ false case _ ⇒ true } })

  def removeSections(prg: Program, sections: Set[String] = Set(".debug_info", ".mdebug.abi32", ".debug_abbrev", ".debug_aranges", ".debug_macinfo", ".debug_line", ".debug_loc", ".debug_pubtypes", ".debug_str", ".debug_ranges")) = {
    var section = ""
    val it = prg.statements.iterator
    val ret = Buffer[Statement]()
    while (it.hasNext) {
      val s = it.next
      section = s match {
        case Directive(d, _) if (Set("data", "text", "bss", "kdata", "ktext", "kbss", "sdata", "sbss") contains d) ⇒ d
        case Directive("section", LabelRef(s) :: _) ⇒ s
        case _ ⇒ section
      }
      if (!(sections contains section)) {
        ret += s
      }
    }
    Program(ret)
  }

  def simplifyData(prg: Program) =
    Program(prg.statements flatMap { s ⇒
      s match {
        case Directive(d, LabelRef(name) :: size :: rest) if (Set("comm", "lcomm") contains d) ⇒
          Seq(Label(name)) ++ (rest match {
            case IntegerConst(align) :: _ ⇒ Some(Directive("align", Seq(IntegerConst(align match { case 1 ⇒ 0 case 2 ⇒ 1 case 3 ⇒ 2 case 4 ⇒ 2 case _ ⇒ 3 }))))
            case _                        ⇒ None
          }) ++ Seq(Directive("space", Seq(size)))
        case Directive("section", LabelRef(s) :: _) if s.startsWith(".rodata") ⇒ Some(Directive("data", Seq()))
        case s ⇒ Some(s)
      }
    })

  def removeUnusedLabels(prg: Program) = {
    val it = prg.statements.iterator
    var usedLabels = Set[String]()
    def examine(operand: Operand): Unit = operand match {
      case LabelRef(l)               ⇒ usedLabels += l
      case IndexedAddress(of, ba)    ⇒ { examine(of); examine(ba) }
      case AssemblerFunction(_, op)  ⇒ examine(op)
      case ArithExpression(op, a, b) ⇒ { examine(a); examine(b) }
      case Parenthesis(e)            ⇒ examine(e)
      case Register(_)               ⇒
      case _: Literal                ⇒
    }
    prg.statements foreach {
      case Directive(_, ops)   ⇒ ops foreach examine
      case Instruction(_, ops) ⇒ ops foreach examine
      case s                   ⇒
    }
    Program(prg.statements flatMap {
      case Label(l) if !(usedLabels contains l) ⇒ None
      case LabelDefinition(l, _) if !(usedLabels contains l) ⇒ None
      case s ⇒ Some(s)
    })
  }

  def renameRegisters(prg: Program) = {
    def renameReg(reg: String) = try (Integer.parseInt(reg) match {
      case 0  ⇒ "0"
      case 1  ⇒ "at"
      case 2  ⇒ "v0"
      case 3  ⇒ "v1"
      case 4  ⇒ "a0"
      case 5  ⇒ "a1"
      case 6  ⇒ "a2"
      case 7  ⇒ "a3"
      case 8  ⇒ "t0"
      case 9  ⇒ "t1"
      case 10 ⇒ "t2"
      case 11 ⇒ "t3"
      case 12 ⇒ "t4"
      case 13 ⇒ "t5"
      case 14 ⇒ "t6"
      case 15 ⇒ "t7"
      case 16 ⇒ "s0"
      case 17 ⇒ "s1"
      case 18 ⇒ "s2"
      case 19 ⇒ "s3"
      case 20 ⇒ "s4"
      case 21 ⇒ "s5"
      case 22 ⇒ "s6"
      case 23 ⇒ "s7"
      case 24 ⇒ "t8"
      case 25 ⇒ "t9"
      case 26 ⇒ "k0"
      case 27 ⇒ "k1"
      case 28 ⇒ "gp"
      case 29 ⇒ "sp"
      case 30 ⇒ "fp"
      case 31 ⇒ "ra"
      case _  ⇒ reg
    }) catch {
      case e: NumberFormatException ⇒ reg
    }
    def rename(operand: Operand): Operand = operand match {
      case Register(name)                   ⇒ Register(renameReg(name))
      case o @ LabelRef(_)                  ⇒ o
      case IndexedAddress(offset, base)     ⇒ IndexedAddress(rename(offset), rename(base))
      case AssemblerFunction(name, operand) ⇒ AssemblerFunction(name, rename(operand))
      case l: Literal                       ⇒ l
      case Parenthesis(op)                  ⇒ Parenthesis(rename(op))
      case ArithExpression(oper, a, b)      ⇒ ArithExpression(oper, rename(a), rename(b))
    }
    Program(prg.statements map {
      case Directive(d, operands)    ⇒ Directive(d, operands map rename)
      case Instruction(i, operands)  ⇒ Instruction(i, operands map rename)
      case s @ Label(_)              ⇒ s
      case s @ LabelDefinition(_, _) ⇒ s
      case s @ Comment(_, _)         ⇒ s
    })
  }

  def groupSections(prg: Program) = {
    var sections = Map.empty[Directive, Buffer[Statement]]
    val order = Buffer(Directive("data", Seq()), Directive("text", Seq()), Directive("kdata", Seq()), Directive("ktext", Seq()))
    var current = Directive("text", Seq())
    def setCurrent(d: Directive) {
      current = d
      if (!(sections contains current)) {
        sections = sections + (current -> Buffer())
        if (!(order contains d)) order += d
      }
    }
    setCurrent(current)
    prg.statements foreach {
      case d @ Directive(s, rest) if (Set("data", "text", "bss", "kdata", "ktext", "kbss", "sdata", "sbss") contains s) ⇒ setCurrent(d)
      case d @ Directive("section", _) ⇒ setCurrent(d)
      case stm ⇒ sections(current) += stm
    }
    val statements = order flatMap { d ⇒
      sections.get(d) match {
        case Some(stms) if stms.size > 0 ⇒ d +: stms
        case _                           ⇒ Seq()
      }
    }
    Program(statements)
  }
}