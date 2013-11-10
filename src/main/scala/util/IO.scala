package util

object IO {
  var unparsed: Option[String] = None
  private def fill() = unparsed match {
    case Some(_) ⇒
    case None ⇒ {
      val l = Console.readLine()
      if (l != null) {
        unparsed = Some(l)
      }
    }
  }
  def eof() = {
    fill()
    unparsed match {
      case Some(_) ⇒ false
      case None    ⇒ true
    }
  }
  def readLine(): String = {
    fill()
    unparsed match {
      case Some(l) ⇒ {
        unparsed = None
        l
      }
      case None ⇒ throw new java.io.EOFException("Console has reached end of input")
    }
  }
  def readWord(): String = {
    fill()
    unparsed match {
      case Some(l) ⇒ {
        val (ret, rest) = l.indexOf(' ') match {
          case i if i >= 0 ⇒ (l.substring(0, i), Some(l.substring(i + 1, l.length)))
          case _           ⇒ (l, None)
        }
        unparsed = rest
        if (ret == "") readWord()
        else ret
      }
      case None ⇒ throw new java.io.EOFException("Console has reached end of input")
    }
  }
  def readInt() = {
    readWord().toInt
  }
}
