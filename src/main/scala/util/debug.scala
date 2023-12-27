package util

object debug {
  var enabled = false

  @inline
  def apply(f: => Unit) = if (enabled) f else ()

  @inline
  def p(x: Any) = debug { Console.println("  " * _indent + x) }

  private var _indent = 0
  def indent[A](f: => A) = {
    _indent += 1
    val r = f
    _indent -= 1
    r
  }
}
