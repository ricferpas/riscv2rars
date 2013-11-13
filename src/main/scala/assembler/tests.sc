package assembler

object tests {
  val c = 0xc3                                    //> c  : Int = 195
  val r = c & 0x1f                                //> r  : Int = 3
  val r6 = (r << 6)                               //> r6  : Int = 192
  val h = 0xb3                                    //> h  : Int = 179
  val v = 0x3f & h                                //> v  : Int = 51
  val rv = r6 | v                                 //> rv  : Int = 243
  f"$rv%02x ${rv.toChar}"                         //> res0: String = f3 ó
}