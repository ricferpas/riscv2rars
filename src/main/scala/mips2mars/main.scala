package mips2mars

import java.io.FileOutputStream
import assembler._
import ast._
import transforms._
import passes._

object passes {
  case class Pass(name: String, transform: Transform)
  val allPasses = Seq(
    Pass("addRuntime", addRuntime),
    Pass("removeSections", removeSections(_)),
    Pass("removeComments", removeComments),
    Pass("removeDirectives", removeDirectives(_)),
    Pass("removeGlobl", removeGlobl),
    Pass("removeDelaySlot", removeDelaySlot),
    Pass("simplifyData", simplifyData),
    Pass("removeUnusedLabels", removeUnusedLabels),
    Pass("renameRegisters", renameRegisters),
    Pass("groupSections", groupSections),
    Pass("removeAlignFromText", removeAlignFromText),
    Pass("simplyfyOperands", simplyfyOperands),
    Pass("simplyfyDirectives", simplyfyDirectives),
    Pass("addLuiPseudoinstructions", addLuiPseudoinstructions),
    Pass("addSimplePseudoinstructions", addSimplePseudoinstructions),
    Pass("avoidRegisterAt", avoidRegisterAt),
    Pass("fixStrings", fixStrings),
    Pass("renameLabels", renameLabels),
    Pass("asciizSpaceDirective", asciizSpaceDirective),
    Pass("addEmptyLines", addEmptyLines),
    Pass("DCE", DCE),
    Pass("addSourceComments", addSourceComments))
}

object Main extends App {
  var outputFile: Option[String] = None
  var inputFile: Option[String] = None
  var debugPassesPrefix: Option[String] = None
  var debugPasses = false
  def debugPassFilename(id: String) = debugPassesPrefix match {
    case Some(p) ⇒ s"${p}-${id}.s"
    case None    ⇒ s"${outputFile.get}-${id}.s"
  }
  var disabledPasses = Set.empty[String]

  def error(e: String) {
    Console.withOut(Console.err) { println(e) }
    sys.exit(1)
  }

  val re_debug_prefix = "--debug-prefix=(.+)".r
  val re_output_file = "--output=(.+)".r
  val re_disable_pass = "--disable-pass=(.+)".r
  val re_any_option = "--(.+)".r
  args.toSeq.foreach {
    case "--list-passes" ⇒ {
      allPasses foreach { p ⇒ println(p.name) }
      sys.exit(0)
    }
    case "--debug-passes"   ⇒ debugPasses = true
    case re_debug_prefix(p) ⇒ debugPassesPrefix = Some(p)
    case re_output_file(f) ⇒ outputFile match {
      case None    ⇒ outputFile = Some(f)
      case Some(_) ⇒ error("Más de un fichero de salida especificado.")
    }
    case re_disable_pass(p) if (allPasses map (_.name)) contains p ⇒ disabledPasses += p
    case re_any_option(o) ⇒ error(s"Opción desconocida $o")
    case f ⇒ inputFile match {
      case None    ⇒ inputFile = Some(f)
      case Some(x) ⇒ error(s"Más de un fichero de entrada especificado «$x» y después «$f».")
    }
  }

  if (debugPasses && debugPassesPrefix == None && outputFile == None)
    error("--debug-passes sin --debug-prefix= ni --output=.")

  if (inputFile == None) error("Fichero de entrada no especificado.")
  val p0 = ast.Program.fromFile(inputFile.get)
  if (debugPasses) {
    val out = new FileOutputStream(debugPassFilename("00-initial"))
    try Console.withOut(out) { println(printer.format(p0)) }
    finally out.close()
  }

  val result = allPasses.filter { case Pass(n, _) ⇒ !(disabledPasses contains n) }.zipWithIndex.foldLeft(p0) {
    case (acc, (passes.Pass(name, fn), idx)) ⇒ {
      val q = fn(acc)
      if (debugPasses) {
        val out = new FileOutputStream(debugPassFilename(f"${idx + 1}%02d-$name"))
        try Console.withOut(out) { println(printer.format(q)) }
        finally out.close()
      }
      q
    }
  }

  val out = outputFile match {
    case Some(n) ⇒ new FileOutputStream(n)
    case None    ⇒ Console.out
  }
  try Console.withOut(out) { println(printer.format(result)) }
  finally outputFile map { n ⇒ out.close() }
}
