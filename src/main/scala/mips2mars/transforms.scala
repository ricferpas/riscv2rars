package mips2mars

import scala.collection.mutable.Buffer
import assembler._
import ast._

object transforms {
  type Transform = Program ⇒ Program

  def mapOperands(prg: Program)(fn: Operand ⇒ Operand) = {
    def recurse(o: Operand): Operand = o match {
      case Register(_)                   ⇒ fn(o)
      case LabelRef(_)                   ⇒ fn(o)
      case l: Literal                    ⇒ fn(l)
      case IndexedAddress(offset, base)  ⇒ fn(IndexedAddress(recurse(offset), recurse(base)))
      case AssemblerFunction(name, oper) ⇒ fn(AssemblerFunction(name, recurse(oper)))
      case ArithExpression(oper, a, b)   ⇒ fn(ArithExpression(oper, recurse(a), recurse(b)))
      case Parenthesis(op)               ⇒ fn(Parenthesis(recurse(op)))
    }
    Program(prg.statements map {
      case Directive(d, operands)   ⇒ Directive(d, operands map recurse)
      case Instruction(i, operands) ⇒ Instruction(i, operands map recurse)
      case s                        ⇒ s
    })
  }

  def foreachOperand(prg: Program)(fn: Operand ⇒ Unit) = {
    def recurse(o: Operand): Unit = o match {
      case Register(_) ⇒ fn(o)
      case LabelRef(_) ⇒ fn(o)
      case l: Literal  ⇒ fn(l)
      case IndexedAddress(offset, base) ⇒
        recurse(offset); recurse(base); fn(IndexedAddress(offset, base))
      case AssemblerFunction(name, oper) ⇒
        recurse(oper); fn(AssemblerFunction(name, oper))
      case ArithExpression(oper, a, b) ⇒
        recurse(a); recurse(b); fn(ArithExpression(oper, a, b))
      case Parenthesis(op) ⇒ recurse(op); fn(Parenthesis(op))
    }
    prg.statements foreach {
      case Directive(_, ops)   ⇒ ops foreach recurse
      case Instruction(_, ops) ⇒ ops foreach recurse
      case s                   ⇒
    }
  }

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
    val ret = Buffer[Statement]()
    for (s ← prg.statements) {
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
    Program(prg.statements flatMap {
      case Directive(d, LabelRef(name) :: size :: rest) if (Set("comm", "lcomm") contains d) ⇒
        Seq(Label(name)) ++ (rest match {
          case IntegerConst(align) :: _ ⇒ Some(Directive("align", Seq(IntegerConst(align match { case 1 ⇒ 0 case 2 ⇒ 1 case 3 ⇒ 2 case 4 ⇒ 2 case _ ⇒ 3 }))))
          case _                        ⇒ None
        }) ++ Seq(Directive("space", Seq(size)))
      case Directive("section", LabelRef(s) :: _) if s.startsWith(".rodata") ⇒ Some(Directive("data", Seq()))
      case s ⇒ Some(s)
    })

  def removeUnusedLabels(prg: Program) = {
    var usedLabels = Set[String]()
    foreachOperand(prg) {
      case LabelRef(l) ⇒ usedLabels += l
      case _           ⇒
    }
    Program(prg.statements flatMap {
      case Label(l) if !(usedLabels contains l) ⇒ None
      case LabelDefinition(l, _) if !(usedLabels contains l) ⇒ None
      case s ⇒ Some(s)
    })
  }

  def renameRegisters(prg: Program) = {
    def renameReg(reg: String) = try (Integer.parseInt(reg) match {
      case 0 ⇒ "0" case 1 ⇒ "at" case 2 ⇒ "v0" case 3 ⇒ "v1" case 4 ⇒ "a0" case 5 ⇒ "a1" case 6 ⇒ "a2" case 7 ⇒ "a3"
      case 8 ⇒ "t0" case 9 ⇒ "t1" case 10 ⇒ "t2" case 11 ⇒ "t3" case 12 ⇒ "t4" case 13 ⇒ "t5" case 14 ⇒ "t6" case 15 ⇒ "t7"
      case 16 ⇒ "s0" case 17 ⇒ "s1" case 18 ⇒ "s2" case 19 ⇒ "s3" case 20 ⇒ "s4" case 21 ⇒ "s5" case 22 ⇒ "s6" case 23 ⇒ "s7"
      case 24 ⇒ "t8" case 25 ⇒ "t9" case 26 ⇒ "k0" case 27 ⇒ "k1" case 28 ⇒ "gp" case 29 ⇒ "sp" case 30 ⇒ "fp" case 31 ⇒ "ra"
      case _ ⇒ reg
    }) catch {
      case e: NumberFormatException ⇒ reg
    }
    mapOperands(prg) {
      case Register(name) ⇒ Register(renameReg(name))
      case o              ⇒ o
    }
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

  def removeAlignFromText(prg: Program) = {
    val textDirective = Directive("text", Seq())
    var current = textDirective
    def setCurrent(d: Directive) {
      current = d
    }
    setCurrent(current)
    val statements = prg.statements flatMap {
      case d @ Directive(s, rest) if (Set("data", "text", "bss", "kdata", "ktext", "kbss", "sdata", "sbss") contains s) ⇒
        setCurrent(d); Some(d)
      case d @ Directive("section", _) ⇒
        setCurrent(d); Some(d)
      case d @ Directive("align", _) ⇒
        if (current == textDirective) None else Some(d)
      case stm ⇒ Some(stm)
    }
    Program(statements)
  }

  def addLuiPseudoinstructions(prg: Program) = {
    val it = prg.statements.iterator.buffered
    val ret = Buffer[Statement]()
    /* El siguiente mapa es necesario porque a veces se reusa un registro después de cargar en el la parte alta de una constante. Además, el  lui y la segunda instrucción pueden no estar contiguos.*/
    /* El algoritmo no es totalmente correcto y puede hacer subtituciones erróneas, pero es poco probable que pase con el código generado por llvm */
    var luiRegs = Map.empty[String, Operand]
    while (it.hasNext) {
      val stmt = it.next
      stmt match {
        case Instruction("lui", Seq(Register(regLui), AssemblerFunction("hi", imm @ LabelRef(label)))) ⇒ {
          luiRegs = luiRegs + (regLui -> imm) // no se añade el lui, se asume que se usará sólo para instrucciones que se substituirán luego (probablemente la próxima)
        }
        case Instruction("addiu", Seq(Register(regDst), Register(regLui), AssemblerFunction("lo", imm @ LabelRef(label)))) if luiRegs.contains(regLui) && luiRegs(regLui) == imm ⇒ {
          ret += Instruction("la", Seq(Register(regDst), imm))
        }
        case Instruction(inst, Seq(Register(regOther), IndexedAddress(AssemblerFunction("lo", imm @ LabelRef(label)), Register(regLui)))) if luiRegs.contains(regLui) && luiRegs(regLui) == imm ⇒ {
          ret += Instruction(inst, Seq(Register(regOther), imm))
        }
        case Instruction("addu", Seq(Register(regTmp), Register(regTmp2), Register(regLui))) if regTmp == regTmp2 && luiRegs.contains(regLui) && it.hasNext ⇒ {
          it.head match {
            case Instruction(inst, Seq(Register(regOther), IndexedAddress(AssemblerFunction("lo", imm @ LabelRef(label)), Register(regTmp3)))) if regTmp3 == regTmp && luiRegs.contains(regLui) && luiRegs(regLui) == imm ⇒ {
              ret += Instruction(inst, Seq(Register(regOther), IndexedAddress(imm, Register(regTmp))))
              it.next // consume head
            }
            case _ ⇒ ret += stmt
          }
        }
        case _ ⇒ ret += stmt
      }
    }
    Program(ret)
  }

  def simplyfyOperands(prg: Program) = mapOperands(prg) {
    case IndexedAddress(offset, Register("gp")) ⇒ offset
    case AssemblerFunction("gp_rel", o)         ⇒ o
    case o                                      ⇒ o
  }

  def simplyfyDirectives(prg: Program) = {
    def removeParenthesis(o: Operand) = o match {
      case Parenthesis(o) ⇒ o
      case o              ⇒ o
    }
    def simplyfy(directive: Directive): Directive = directive match {
      case Directive("4byte", ops) ⇒ Directive("word", ops map removeParenthesis)
      case Directive("asciz", ops) ⇒ Directive("asciiz", ops)
      case d                       ⇒ d
    }
    Program(prg.statements map {
      case s @ Directive(_, _) ⇒ simplyfy(s)
      case s                   ⇒ s
    })
  }

  def avoidRegisterAt(prg: Program) = {
    var usedRegisters = Set.empty[String]
    foreachOperand(prg) {
      case Register(r) ⇒ usedRegisters += r
      case _           ⇒
    }
    Seq("t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t1", "t2", "gp", "fp", "k0", "k1").find(!usedRegisters(_)) match {
      case Some(alternative) ⇒ mapOperands(prg) {
        case Register("at") ⇒ Register(alternative)
        case o              ⇒ o
      }
      case None ⇒ prg
    }
  }

  def fixStrings(prg: Program) = {
    Program(prg.statements map {
      case Directive("asciiz", strings) ⇒ Directive("asciiz", strings map {
        case StringConst(s) ⇒ StringConst {
          val sb = new StringBuilder()
          val it = s.iterator.buffered
          while (it.hasNext) {
            sb += (it.next.toInt match {
              case c if (c & 0x80) != 0 ⇒ {
                var r = c & 0x1f
                def h = it.head.toInt
                while (it.hasNext && ((h & 0xc0) == 0x80)) {
                  r = (r << 6) | (0x3f & h)
                  it.next
                }
                r.toChar
              }
              case c ⇒ c.toChar
            })
          }
          sb.result
        }
        case op ⇒ op
      })
      case stm ⇒ stm
    })
  }

  def renameLabels(prg: Program) = {
    var indexedNames = Map.empty[String, Int].withDefaultValue(0)
    def indexedName(n: String) = {
      val r = indexedNames(n)
      indexedNames += n → (r + 1)
      f"$n$r%03d"
    }
    val labels = prg.statements flatMap {
      case Label(l)              ⇒ Some(l)
      case LabelDefinition(l, _) ⇒ Some(l)
      case _                     ⇒ None
    }
    var usedLabels = labels.toSet
    var subs = Map.empty[String, String]
    def rename(l: String) = subs.getOrElse(l, {
      var sub =
        if (l matches """\$\.str.*""") {
          indexedName("str")
        } else if (l matches """\$JTI.*""") {
          indexedName("jump_table")
        } else if (l matches """\$BB.*""") {
          l.substring(2)
        } else l
      while ((usedLabels contains sub) && (sub != l)) sub = sub + "X"
      usedLabels += sub
      subs += l → sub
      sub
    })

    mapOperands(
      Program(prg.statements map {
        case Label(l)              ⇒ Label(rename(l))
        case LabelDefinition(l, v) ⇒ LabelDefinition(rename(l), v)
        case s                     ⇒ s
      })) {
        case LabelRef(l) ⇒ LabelRef(subs.getOrElse(l, l))
        case o           ⇒ o
      }
  }

  def addEmptyLines(prg: Program) = {
    val procedures = {
      var ps = Set("main")
      prg.statements foreach {
        case Instruction("jal", Seq(LabelRef(l))) ⇒ ps += l
        case _                                    ⇒
      }
      ps
    }
    val stmts = Buffer.empty[Statement]
    val currentGroup = Buffer.empty[Statement]
    for (s ← prg.statements) s match {
      case Label(l) if procedures contains l ⇒ {
        stmts += EmptyLine
        stmts ++= currentGroup
        currentGroup.clear
        stmts += s
      }
      case Directive(d, _) if Set("data", "text", "section") contains d ⇒ {
        stmts ++= currentGroup
        currentGroup.clear
        stmts += EmptyLine
        stmts += s
      }
      case Directive(_, _) ⇒ {
        currentGroup += s
      }
      case _ ⇒ {
        stmts ++= currentGroup
        currentGroup.clear
        stmts += s
      }
    }
    stmts ++= currentGroup
    Program(stmts)
  }
}
