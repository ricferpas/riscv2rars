package assembler

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Map ⇒ MMap, Set ⇒ MSet }

object linker {
  type Address = Long
  type Offset = Int
  type Word = Int

  class Linkable {
    val sections = ArrayBuffer[Section]()
    val labels = MMap[String, SectionRef]()
    val globals = MSet[String]()

    def addSection(tipe: SectionType.Value = SectionType.Text, address: Option[Address] = None) = {
      val res = new Section(tipe)
      sections += res
      res
    }

    def addLabel(name: String, dest: SectionRef) { labels(name) = dest }
    def addGlobal(name: String) { globals += name }
  }

  class Section(val tipe: SectionType.Value = SectionType.Text,
                var address: Option[Address] = None) {
    val data = ArrayBuffer[Byte]()
    val relocations = MMap[Offset, Relocation]()

    def append(b: Byte) { data += b }
    def appendSpace(s: Offset) { data ++= Iterator.fill(s) { 0 } }
    def appendWord(w: Word) {
      data += (w & 0xff).toByte
      data += ((w >> 8) & 0xff).toByte
      data += ((w >> 16) & 0xff).toByte
      data += ((w >> 24) & 0xff).toByte
    }
    def appendIInst(op: Int, rs: Int, rt: Int, imm: Int) =
      appendWord(((op & 0x3f) << 26) | ((rs & 0x1f) << 21) | ((rt & 0x1f) << 16) | (imm & 0xff))

    def position: Offset = data.size

    def addRelocation(rel: Relocation, pos: Offset = position) { relocations(pos) = rel }
  }

  object SectionType extends Enumeration {
    val Text = Value
    val Data = Value
  }

  case class Relocation(target: Reference, tipe: RelocationType.Value = RelocationType.immBoth)

  object RelocationType extends Enumeration {
    val immHigh = Value
    val immLow = Value
    val immBoth = Value
  }

  abstract class Reference
  case class LabelRef(label: String, offset: Offset) extends Reference
  case class SectionRef(section: Section, offset: Offset) extends Reference
}
