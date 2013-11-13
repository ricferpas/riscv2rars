package util

object debug {
  var enabled = false
  
  @inline 
  def apply(f: ⇒ Unit) = if (enabled) f else ()
}
