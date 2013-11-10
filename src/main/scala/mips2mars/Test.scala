package mips2mars

import assembler._
import java.io.FileOutputStream
import scala.collection.mutable.Buffer
import ast._

object transforms {
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
}

class Test1 extends App {
  val inputFilename = args(0)
  val outputFilenamePrefix = args(1)

  def outputFilename(id: String) = s"${outputFilenamePrefix}-${id}.s"

  val p0 = ast.Program.fromFile(inputFilename)
  Console.withOut(new FileOutputStream(outputFilename("0"))) { println(printer.format(p0)) }

  import transforms._

  Seq[(String, Program ⇒ Program)](
    ("removeSections", removeSections(_)),
    ("removeComments", removeComments),
    ("removeDirectives", removeDirectives(_)),
    ("removeDelaySlot", removeDelaySlot),
    ("simplifyData", simplifyData),
    ("removeUnusedLabels", removeUnusedLabels)).zipWithIndex.foldLeft(p0) {
      case (acc, ((name, fn), idx)) ⇒ {
        val q = fn(acc)
        Console.withOut(new FileOutputStream(outputFilename(s"${idx + 1}-$name"))) { println(printer.format(q)) }
        q
      }
    }
}

object Test extends Test1
