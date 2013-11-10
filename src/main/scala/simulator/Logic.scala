package simulator

class Logic extends Simulator with Signals {
  object config {
    val D = 3
  }

  type Wire = Input[Boolean]

  trait Comp {
    val inputs: Seq[Socket[Boolean]]
    def x = inputs(0)
    def y = inputs(1)
    def z = inputs(2)
    def out: Wire
  }

  class Switch(_state: Boolean) extends Comp {
    val state = Var(_state)
    val inputs = List.empty
    def out: Wire = state
  }
  def Switch(v: Boolean) = new Switch(v)

  abstract class ReducingGate(ws: Wire*) extends Comp {
    val inputs = ws map { Socket(_) }
    def expr(a: Boolean, b: Boolean): Boolean
    def out = Expr(inputs: _*) { inputs.tail.foldLeft(inputs.head()) { (acc, sig) ⇒ expr(acc, sig()) } } delayed config.D
  }
  object ReducingGate {
    def apply(ws: Wire*)(e: (Boolean, Boolean) ⇒ Boolean) = new ReducingGate(ws: _*) {
      def expr(a: Boolean, b: Boolean): Boolean = e(a, b)
    }
  }

  def Nand(ws: Wire*) = ReducingGate(ws: _*) { (a, b) ⇒ !(a && b) }
  def Nor(ws: Wire*) = ReducingGate(ws: _*) { (a, b) ⇒ !(a || b) }
  def Or(ws: Wire*) = ReducingGate(ws: _*) { (a, b) ⇒ a || b }
  def And(ws: Wire*) = ReducingGate(ws: _*) { (a, b) ⇒ a && b }

  class Not(val in: Socket[Boolean]) extends Comp {
    val inputs = Seq(in)
    val out = Expr(in) { !in() } delayed config.D
  }
  def Not(w: Wire) = new Not(Socket(w))
}

