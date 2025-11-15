package riscv2rars

import assembler.ast._
import util.debug
import util.misc._

import scala.collection.mutable
import scala.util.Using

object transforms {
  type Transform = Program => Program

  def mapOperands(prg: Program)(fn: Operand => Operand) = {
    def recurse(o: Operand): Operand = o match {
      case Register(_)                   => fn(o)
      case LabelRef(_)                   => fn(o)
      case l: Literal                    => fn(l)
      case IndexedAddress(offset, base)  => fn(IndexedAddress(recurse(offset), recurse(base)))
      case AssemblerFunction(name, oper) => fn(AssemblerFunction(name, recurse(oper)))
      case ArithExpression(oper, a, b)   => fn(ArithExpression(oper, recurse(a), recurse(b)))
      case Parenthesis(op)               => fn(Parenthesis(recurse(op)))
    }
    Program(prg.statements map {
      case Directive(d, operands)   => Directive(d, operands map recurse)
      case Instruction(i, operands) => Instruction(i, operands map recurse)
      case s                        => s
    })
  }

  def foreachOperand(prg: Program)(fn: Operand => Unit) = {
    def recurse(o: Operand): Unit = o match {
      case Register(_) => fn(o)
      case LabelRef(_) => fn(o)
      case l: Literal  => fn(l)
      case IndexedAddress(offset, base) =>
        recurse(offset); recurse(base); fn(IndexedAddress(offset, base))
      case AssemblerFunction(name, oper) =>
        recurse(oper); fn(AssemblerFunction(name, oper))
      case ArithExpression(oper, a, b) =>
        recurse(a); recurse(b); fn(ArithExpression(oper, a, b))
      case Parenthesis(op) => recurse(op); fn(Parenthesis(op))
    }
    prg.statements foreach {
      case Directive(_, ops)   => ops foreach recurse
      case Instruction(_, ops) => ops foreach recurse
      case _                   =>
    }
  }

  def removeDirectives(prg: Program, directives: Set[String] = Set(
    "set", "mask", "fmask", "frame", "cfi_sections", "cfi_startproc", "cfi_endproc", "cfi_def_cfa_offset", "cfi_offset",
    "ent", "type", "end", "previous", "size", "local", "ident", "file", "option", "loc", "cfi_restore", "cfi_remember_state",
    "cfi_restore_state", "attribute")): Program =
    Program(prg.statements.filter {
      case Directive(d, _) if directives contains d => false
      case _ => true
    })

  def removeGlobl(prg: Program) = Program(prg.statements filter {
    case Directive("globl", Seq(LabelRef("main"))) => true
    case Directive("globl", _)                     => false
    case _                                         => true
  })

  def removeComments(prg: Program) =
    Program(prg.statements filter { case Comment(_, _) => false case _ => true })

  def removeSections(prg: Program, sections: Set[String] = Set(
    ".debug_info", ".mdebug.abi32", ".debug_abbrev", ".debug_aranges", ".debug_macinfo", ".debug_line", ".debug_loc",
    ".debug_pubtypes", ".debug_str", ".debug_ranges", ".debug_loclists", ".debug_rnglists", ".debug_line_str")): Program = {
    var section = ""
    val ret = mutable.Buffer.empty[Statement]
    prg.statements foreach { s =>
      section = s match {
        case Directive(d, _) if Set("data", "text", "bss", "kdata", "ktext", "kbss", "sdata", "sbss") contains d => d
        case Directive("section", LabelRef(s) :: _) => s
        case _ => section
      }
      if (!(sections contains section)) {
        ret += s
      }
    }
    Program(ret)
  }

  def simplifyData(prg: Program) = {
    val ret = mutable.Buffer.empty[Statement]
    val new_data = mutable.Buffer.empty[Statement]
    prg.statements foreach {
      case Directive(d, LabelRef(name) :: size :: rest) if Set("comm", "lcomm") contains d =>
        new_data += Label(name)
        rest match {
          case IntegerConst(align, albase) :: _ =>
            new_data += Directive("align", Seq(IntegerConst(align match { case 1 => 0 case 2 => 1 case 3 => 2 case 4 => 2 case _ => 3 }, albase)))
          case _ =>
        }
        new_data += Directive("space", Seq(size))
      case Directive("section", LabelRef(s) :: _) if s.matches("""\..{0,3}data.*""") => ret += Directive("data", Seq())
      case Directive("section", LabelRef(".sbss") :: _) => ret += Directive("data", Seq())
      case Directive("bss", _) => ret += Directive("data", Seq())
      case s => ret += s
    }
    if (new_data.nonEmpty) {
      ret += Directive("data", Seq())
      ret ++= new_data
    }
    Program(ret)
  }

  def removeUnusedLabels(prg: Program) = {
    var usedLabels = Set[String]()
    foreachOperand(prg) {
      case LabelRef(l) => usedLabels += l
      case _           =>
    }
    Program(prg.statements flatMap {
      case Label(l) if !(usedLabels contains l) => None
      case LabelDefinition(l, _) if !(usedLabels contains l) => None
      case s => Some(s)
    })
  }

  def renameRegisters(prg: Program) = {
    def renameReg(reg: String) = reg // TODO
    mapOperands(prg) {
      case Register(name) => Register(renameReg(name))
      case o              => o
    }
  }

  val sectionChangingDirectives = Set("data", "text", "section", "bss", "kdata", "ktext", "kbss", "sdata", "sbss")

  def groupSections(prg: Program): Program = {
    var sections = Map.empty[Directive, mutable.Buffer[Statement]]
    val order = mutable.Buffer(Directive("data", Seq()), Directive("text", Seq()), Directive("kdata", Seq()), Directive("ktext", Seq()))
    var current = Directive("text", Seq())
    def setCurrent(d: Directive): Unit = {
      current = d
      if (!(sections contains current)) {
        sections = sections + (current -> mutable.Buffer.empty)
        if (!(order contains d)) order += d
      }
    }
    setCurrent(current)
    prg.statements.foreach {
      case d @ Directive(s, _) if sectionChangingDirectives contains s => setCurrent(d)
      case stm => sections(current) += stm
    }
    val statements = order.flatMap { d =>
      sections.get(d) match {
        case Some(stms) if stms.nonEmpty => d +: stms
        case _                           => Seq()
      }
    }
    Program(statements)
  }

  def removeAlignFromText(prg: Program) = {
    val textDirective = Directive("text", Seq())
    var current = textDirective
    def setCurrent(d: Directive) = {
      current = d
    }
    setCurrent(current)
    val statements = mutable.Buffer.empty[Statement]
    prg.statements foreach {
      case d @ Directive(s, _) if Set("data", "text", "section", "bss", "kdata", "ktext", "kbss", "sdata", "sbss") contains s =>
        setCurrent(d)
        statements += d
      case d @ Directive("align", _) =>
        if (current != textDirective) statements += d
      case stm =>
        statements += stm
    }
    Program(statements)
  }

  def addAddressPseudoinstructions(prg: Program) = {
    val it = prg.statements.iterator.buffered
    val ret = mutable.Buffer.empty[Statement]
    /* El siguiente mapa es necesario porque a veces se reusa un registro después de cargar en el la parte alta de una constante. Además, el  lui y la segunda instrucción pueden no estar contiguos.*/
    /* El algoritmo no es totalmente correcto y puede hacer subtituciones erróneas, pero es poco probable que pase con el código generado por llvm */
    var luiRegs = Map.empty[String, Operand]
    while (it.hasNext) {
      val stmt = it.next()
      stmt match {
        case Instruction("lui", Seq(Register(regLui), AssemblerFunction("hi", imm @ LabelRef(_)))) =>
          luiRegs = luiRegs + (regLui -> imm) // no se añade el lui, se asume que se usará sólo para instrucciones que se substituirán luego (probablemente la próxima)

        case Instruction("lui", Seq(Register(regLui), AssemblerFunction("hi", expr @ ArithExpression(op, LabelRef(label), offset)))) =>
          luiRegs = luiRegs + (regLui -> expr) // no se añade el lui, se asume que se usará sólo para instrucciones que se substituirán luego (probablemente la próxima)

        case Instruction("addi", Seq(Register(regDst), Register(regLui), AssemblerFunction("lo", expr))) if luiRegs.contains(regLui) && luiRegs(regLui) == expr =>
          ret += Instruction("la", Seq(Register(regDst), expr))

        case Instruction(inst, Seq(Register(regOther), IndexedAddress(AssemblerFunction("lo", expr), Register(regLui)))) if luiRegs.contains(regLui) && luiRegs(regLui) == expr =>
          inst match {
            case "lw" | "lh" | "lhu" | "lb" | "lbu" | "flw" =>
              ret += Instruction(inst, Seq(Register(regOther), expr))
            case "sw" | "sh" | "sb" | "fsw" =>
              ret += Instruction(inst, Seq(Register(regOther), expr, Register(regLui)))
            case _ =>
              println(s"$inst $regOther $expr $regLui")
              ???
          }

        case Instruction("add", Seq(Register(regTmp), Register(regTmp2), Register(regLui))) if regTmp == regTmp2 && luiRegs.contains(regLui) && it.hasNext =>
          it.head match {
            case Instruction(inst, Seq(Register(regOther), IndexedAddress(AssemblerFunction("lo", expr), Register(regTmp3)))) if regTmp3 == regTmp && luiRegs.contains(regLui) && luiRegs(regLui) == expr =>
              ret += Instruction(inst, Seq(Register(regOther), IndexedAddress(expr, Register(regTmp))))
              it.next() // consume head
            case _ => ret += stmt
          }

        case Instruction("lla", operands) => ret += Instruction("la", operands)
        case _ => ret += stmt
      }
    }
    Program(ret)
  }

  def addSimplePseudoinstructions(prg: Program) = {
    Program(prg.statements map {
      case Instruction("addi", Seq(Register(regDst), Register("zero"), IntegerConst(imm, immb))) => Instruction("li", Seq(Register(regDst), IntegerConst(imm, immb)))
      case Instruction("beq", Seq(Register(srcDst), Register("zero"), dest)) => Instruction("beqz", Seq(Register(srcDst), dest))
      case Instruction("bne", Seq(Register(srcDst), Register("zero"), dest)) => Instruction("bnez", Seq(Register(srcDst), dest))
      case Instruction("jr", Seq(Register(r))) if Set("ra", "x1") contains r => Instruction("ret", Seq())
      case s => s
    })
  }

  case class Pseudo(i1n: String, i1a: Seq[String], i2n: String, i2a: Seq[String], repln: String, repla: Seq[String]) {
    def apply(i1: Instruction, i2: Instruction): Option[Instruction] = {
      if (i1.opcode == i1n && i2.opcode == i2n && i1.operands.size == i1a.size && i2.operands.size == i2a.size) {
        var ops = (i1a zip i1.operands).toMap
        val conflicts = (i2a zip i2.operands) filter {
          case (a, o) => (ops contains a) && (o != ops(a))
        }
        if (conflicts.isEmpty) {
          ops = ops ++ (i2a zip i2.operands)
          Some(Instruction(repln, repla map ops))
        } else None
      } else None
    }
  }
  def Pseudo(p: String): Pseudo = {
    val Array(i1p, i2p, replp) = p.split(",", 3)
    val Array(i1n, i1a) = i1p.split(" ", 2)
    val Array(i2n, i2a) = i2p.split(" ", 2)
    val Array(repln, repla) = replp.split(" ", 2)
    Pseudo(i1n, i1a.split(" ").toSeq, i2n, i2a.split(" ").toSeq, repln, repla.split(" ").toSeq)
  }
  val pseudos = Set("slti t a b, bnez t l, blt a b l").map(Pseudo)

  def addComplexPseudoinstructions(prg: Program) = {
    var producers = Map.empty[String, Statement].withDefaultValue(EmptyLine)
    val ret = mutable.Buffer.empty[Statement]
    prg.statements foreach { s =>
      for (case Register(r) <- s.outputOperands) producers += r -> s
      ret += (s match {
        case Label(_) =>
          producers = producers.empty; s
        case Instruction("bnez", Seq(Register(srcTmp), dest)) => producers(srcTmp) match {
          case Instruction("slti", Seq(Register(srcTmp2), opA, opB)) if srcTmp == srcTmp2 && opA != Register(srcTmp) && opB != Register(srcTmp) =>
            Instruction("blt", Seq(opA, opB, dest))
          case _ => s
        }
        case s => s
      })
    }
    Program(ret)
  }

  def simplyfyOperands(prg: Program) = mapOperands(prg) { // TODO (mips)
    case IndexedAddress(offset, Register("gp")) => offset
    case AssemblerFunction("gp_rel", o)         => o
    case o                                      => o
  }

  def simplyfyDirectives(prg: Program) = {
    def removeParenthesis(o: Operand) = o match {
      case Parenthesis(o) => o
      case o              => o
    }
    Program(prg.statements map {
      case Directive("4byte", ops) => Directive("word", ops map removeParenthesis)
      case Directive("asciz", ops) => Directive("string", ops)
      case Directive("asciiz", ops) => Directive("string", ops)
      case Directive("zero", Seq(IntegerConst(n,nb))) => Directive("space", Seq(IntegerConst(n,nb)))
      case Directive("set", Seq(LabelRef(l), ArithExpression("+", LabelRef("."), IntegerConst(0, _)))) => Label(l)
      case Directive("section", LabelRef(".text.startup") :: _) => Directive("text", Seq())
      case s => s
    })
  }

  def fixStrings(prg: Program): Program = {
    def bytesUTF8toString(s:String):String = {
      val sb = new StringBuilder()
      val it = s.iterator.buffered
      while (it.hasNext) {
        sb += (it.next().toInt match {
          case c if (c & 0x80) != 0 =>
            var r = c & 0x1f
            def h = it.head.toInt
            while (it.hasNext && ((h & 0xc0) == 0x80)) {
              r = (r << 6) | (0x3f & h)
              it.next()
            }
            r.toChar
          case c => c.toChar
        })
      }
      sb.result()
    }
    Program(prg.statements map {
      case Directive("string", strings) => Directive("string", strings map {
        case StringConst(s) => StringConst(bytesUTF8toString(s))
        case op => op
      }) // TODO: ascii and acsciz shold be treated likewise
      case Directive("base64", strings) =>
        val ts = strings map {
          case StringConst(s) => StringConst(new String(java.util.Base64.getDecoder.decode(s), "UTF-8"))
          case op => op
        }
        if (ts.forall { case StringConst(s) if s.nonEmpty && s.last == '\u0000' => true case _ => false })
          Directive("string", ts map {
            case StringConst(s) => StringConst(s.dropRight(1))
            case op => op
          })
        else
          Directive("ascii", ts)
      case stm => stm
    })
  }

  def renameLabels(prg: Program) = {
    var indexedNames = Map.empty[String, Int].withDefaultValue(0)
    def indexedName(n: String) = {
      val r = indexedNames(n)
      indexedNames += n -> (r + 1)
      f"$n$r%03d"
    }
    val labels = prg.statements flatMap {
      case Label(l)              => Some(l)
      case LabelDefinition(l, _) => Some(l)
      case _                     => None
    }
    var usedLabels = labels.toSet
    var subs = Map.empty[String, String]
    def rename(l: String) = subs.getOrElse(l, {
      var sub =
        if (l.matches("""\.str.*""")) {
          indexedName("str")
        } else if (l.matches("""JTI.*""")) {
          indexedName("jump_table")
        } else if (l.matches("""\.L[0-9]+""")) {
          indexedName("L")
        } else if (l.matches("""\.LC[0-9]+""")) {
          indexedName("LC")
        } else l
      while ((usedLabels contains sub) && (sub != l)) sub = sub + "X"
      usedLabels += sub
      subs += l -> sub
      sub
    })

    mapOperands(
      prg.copy(statements = prg.statements map {
        case Label(l)              => Label(rename(l))
        case LabelDefinition(l, v) => LabelDefinition(rename(l), v)
        case s                     => s
      })) {
        case LabelRef(l) => LabelRef(subs.getOrElse(l, l))
        case o           => o
      }
  }

  def addEmptyLines(prg: Program) = {
    val procedures = {
      var ps = Set("main")
      prg.statements foreach {
        case Instruction("jal", Seq(LabelRef(l))) => ps += l
        case Instruction("call", Seq(LabelRef(l))) => ps += l
        case Instruction("tail", Seq(LabelRef(l))) => ps += l
        case _                                    =>
      }
      ps
    }
    val stmts = mutable.Buffer.empty[Statement]
    val currentGroup = mutable.Buffer.empty[Statement]
    for (s <- prg.statements) s match {
      case Label(l) if procedures contains l =>
        stmts += EmptyLine
        stmts ++= currentGroup
        currentGroup.clear()
        stmts += s
      case Directive(d, _) if Set("data", "text", "section") contains d =>
        stmts ++= currentGroup
        currentGroup.clear()
        stmts += EmptyLine
        stmts += s
      case Directive(_, _) =>
        currentGroup += s
      case _ =>
        stmts ++= currentGroup
        currentGroup.clear()
        stmts += s
    }
    stmts ++= currentGroup
    Program(stmts)
  }

  def addRuntime(prg: Program) = {
    val reader = new java.io.InputStreamReader(getClass.getResourceAsStream("/runtime/rars-runtime.s"))
    val runtime = Program.fromReader(reader)
    reader.close()
    Program(prg.statements ++ runtime.statements)
  }

  def stringSpaceDirective(prg: Program) = {
    val re = "(.+\u0000+)".r
    Program(prg.statements flatMap {
      case Directive("string", Seq(StringConst(re(s)))) =>
        val bytes = s.getBytes("UTF-8")
        val numZeros = bytes.length - bytes.lastIndexWhere(_ != 0) - 1 // no contar el \0 final, ya incluido al usar asciiz
        if (numZeros > 30) Seq(Directive("string", Seq(StringConst(new String(bytes, 0, bytes.length - numZeros, "UTF-8")))), Directive("space", Seq(IntegerConst(numZeros, 10))))
        else Seq(Directive("string", Seq(StringConst(s))))
      case s => Seq(s)
    })
  }

  //def DCE(prg: Program) =  // TODO: mips

  def warning(e: String): Unit = {
    Console.withOut(Console.err) { println(e) }
  }

  def addSourceComments(prg: Program) = {
    val it = prg.statements.iterator
    val ret = mutable.Buffer[Statement]()
    var curr = Seq.empty[(Int, Int)]
    var files = Map.empty[Int, IndexedSeq[String]]
    def addFile(id: Int, fname: String) = try {
      Using(io.Source.fromFile(fname)) { src =>
        files = files + (id -> (IndexedSeq.empty ++ src.getLines().map { s => if (s.length >= 2 && s.substring(0, 2) == "  ") s.substring(2) else s }))
      }
    } catch {
      case _: java.io.FileNotFoundException => warning(s"Source file no encontrado: $fname")
    }
    def getLine(f: Int, l: Int) = try files(f)(l - 1) catch {
      case _: NoSuchElementException    => ""
      case _: IndexOutOfBoundsException => ""
    }
    while (it.hasNext) {
      val s = it.next()
      s match {
        case Directive("file", Seq(IntegerConst(id, _), StringConst(fname))) => addFile(id.toInt, fname)
        case Directive("loc", Seq(IntegerConst(file, _), IntegerConst(line, _), _)) => curr = curr :+ (file.toInt, line.toInt)
        case Directive("loc", Seq(IntegerConst(file, _), IntegerConst(line, _), _, _)) => curr = curr :+ (file.toInt, line.toInt)
        case Directive(_, _) => ret += s
        case _ =>
          ret += s
          if (curr.nonEmpty) {
            curr.sorted.filterDuplicates foreach {
              case (f, l) =>
                ret += Comment(f"$l%4d ${getLine(f, l)} ", attached = true)
            }
            curr = Seq.empty
          }
      }
    }
    Program(ret)
  }
}
