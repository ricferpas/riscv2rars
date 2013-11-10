package mips2mars

import java.io.FileOutputStream
import assembler._
import ast._
import transforms._

class Test1 extends App {
  val inputFilename = args(0)
  val outputFilenamePrefix = args(1)

  def outputFilename(id: String) = s"${outputFilenamePrefix}-${id}.s"

  val p0 = ast.Program.fromFile(inputFilename)
  Console.withOut(new FileOutputStream(outputFilename("0"))) { println(printer.format(p0)) }

  Seq[(String, Transform)](
    ("removeSections", removeSections(_)),
    ("removeComments", removeComments),
    ("removeDirectives", removeDirectives(_)),
    ("removeDelaySlot", removeDelaySlot),
    ("simplifyData", simplifyData),
    ("removeUnusedLabels", removeUnusedLabels),
    ("renameRegisters", renameRegisters),
    ("groupSections", groupSections))
    .zipWithIndex.foldLeft(p0) {
      case (acc, ((name, fn), idx)) ⇒ {
        val q = fn(acc)
        val out = new FileOutputStream(outputFilename(s"${idx + 1}-$name"))
        Console.withOut(out) { println(printer.format(q)) }
        out.close()
        q
      }
    }
}

object Test extends Test1
