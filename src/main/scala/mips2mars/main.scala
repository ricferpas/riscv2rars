package mips2mars

import java.io.FileOutputStream
import assembler._
import ast._
import transforms._

object Main extends App {
  var outputFile: Option[String] = None
  var inputFile: Option[String] = None
  var debugPassesPrefix: Option[String] = None
  var debugPasses = false
  def debugPassFilename(id: String) = debugPassesPrefix match {
    case Some(p) ⇒ s"${p}-${id}.s"
    case None    ⇒ s"${outputFile.get}-${id}.s"
  }

  val re_debug_prefix = "--debug-prefix=(.+)".r
  val re_output_file = "--output=(.+)".r
  args.toSeq.foreach {
    case "--debug-passes"   ⇒ debugPasses = true
    case re_debug_prefix(p) ⇒ debugPassesPrefix = Some(p)
    case re_output_file(f) ⇒ outputFile match {
      case None    ⇒ outputFile = Some(f)
      case Some(_) ⇒ sys.error("Más de un fichero de salida especificado")
    }
    case f ⇒ inputFile match {
      case None    ⇒ inputFile = Some(f)
      case Some(_) ⇒ sys.error("Más de un fichero de entrada especificado")
    }
  }

  val p0 = ast.Program.fromFile(inputFile.get)
  if (debugPasses) {
    val out = new FileOutputStream(debugPassFilename("00-initial"))
    Console.withOut(out) { println(printer.format(p0)) }
    out.close()
  }

  val result = Seq[(String, Transform)](
    ("removeSections", removeSections(_)),
    ("removeComments", removeComments),
    ("removeDirectives", removeDirectives(_)),
    ("removeDelaySlot", removeDelaySlot),
    ("simplifyData", simplifyData),
    ("removeUnusedLabels", removeUnusedLabels),
    ("renameRegisters", renameRegisters),
    ("groupSections", groupSections),
    ("removeAlignFromText", removeAlignFromText),
    ("simplyfyOperands", simplyfyOperands),
    ("simplyfyDirectives", simplyfyDirectives),
    ("addLuiPseudoinstructions", addLuiPseudoinstructions),
    ("avoidRegisterAt", avoidRegisterAt),
    ("fixStrings", fixStrings),
    ("renameLabels", renameLabels),
    ("addEmptyLines", addEmptyLines))
    .zipWithIndex.foldLeft(p0) {
      case (acc, ((name, fn), idx)) ⇒ {
        val q = fn(acc)
        if (debugPasses) {
          val out = new FileOutputStream(debugPassFilename(f"${idx + 1}%02d-$name"))
          Console.withOut(out) { println(printer.format(q)) }
          out.close()
        }
        q
      }
    }

  val out = outputFile match {
    case Some(n) ⇒ new FileOutputStream(n)
    case None    ⇒ Console.out
  }
  Console.withOut(out) { println(printer.format(result)) }
  outputFile map { n ⇒ out.close() }
}
