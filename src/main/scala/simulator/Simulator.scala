package simulator

import scala.collection._
import mutable.PriorityQueue

trait Simulator {
  type Action = () ⇒ Unit
  type Time = Long

  private case class Event(time: Time, action: Action)
  private val eventQueue = PriorityQueue[Event]()(Ordering.fromLessThan[Event](_.time > _.time))
  private var _currentTime: Time = 0
  def currentTime = _currentTime

  def scheduleAction(t: Time, a: Action) {
    assert(currentTime <= t)
    eventQueue += Event(t, a)
  }
  final def schedule(t: Time)(block: ⇒ Unit) = scheduleAction(t, () ⇒ block)
  final def after(delay: Time)(block: ⇒ Unit) = scheduleAction(currentTime + delay, () ⇒ block)

  private def runNext() {
    val wi = eventQueue.dequeue()
    assert(currentTime <= wi.time)
    _currentTime = wi.time
    wi.action()
  }
  private def runWhile(cond: ⇒ Boolean) = while (cond) runNext()
  def run() = runWhile(!eventQueue.isEmpty)
  def advanceTime(t: Time) {
    runWhile(!eventQueue.isEmpty && eventQueue.head.time <= t)
    _currentTime = currentTime max t
  }

  trait EventSource {
    def addAction(action: Action)
    final def ~(action: ⇒ Unit) = addAction(() ⇒ action)
    def actions: List[Action]
    protected def emit() = actions foreach { _() }
  }

  trait EventSourceActionsList { self: EventSource ⇒
    private[this] var _actions = List.empty[Action]
    def actions = _actions
    def addAction(action: Action) {
      _actions = action :: _actions
    }
  }
}

trait Signals { self: Simulator ⇒
  trait Input[T] extends EventSource {
    def value: T
    final def apply() = value
    def delayed(delay: Time): Input[T]
  }

  trait Output[T] {
    def value_=(v: T)
  }

  sealed trait Signal[T] extends Input[T] {
    def delayed(delay: Time): Signal[T] = new DelayedExpr(delay)(Set(this))(value)
  }

  abstract class ActiveSignal[T] extends Signal[T] with EventSourceActionsList

  class Val[T](s: Signal[T]) extends Signal[T] {
    def value = s.value
    def addAction(action: Action) = s.addAction(action)
    def actions = s.actions
  }
  object Val {
    def apply[T](s: Signal[T]) = new Val(s)
  }

  class ErrorSignal[T] private (message: String) extends ActiveSignal[T] {
    override def delayed(delay: Time) = this
    def value: T = sys.error(message)
  }
  object ErrorSignal {
    def apply[T](message: String = "Error signal") = new ErrorSignal[T](message)
  }
  def undefined[T] = ErrorSignal[T]("Undefined signal")
  def ??[T] = undefined[T]

  case class Const[T](value: T) extends ActiveSignal[T] {
    override def delayed(delay: Time) = this
  }

  class Var[T] private (private[this] var _value: T) extends ActiveSignal[T] with Output[T] {
    def value = _value
    def value_=(v: T) {
      if (_value != v) {
        _value = v
        emit()
      }
    }
    final def :=(v: T) = value = v
    after(0) { emit() }
  }
  object Var {
    def apply[T](v: T) = new Var[T](v)
  }

  class Expr[T] protected (depends: Set[EventSource])(expr: ⇒ T) extends ActiveSignal[T] {
    override def delayed(delay: Time) = new DelayedExpr(delay)(depends)(expr)
    private[this] var _value: T = _
    def value = _value
    protected def updateValue(v: T) {
      if (_value != v) {
        _value = v
        emit()
      }
    }
    protected def updated {
      updateValue(expr)
    }
    depends foreach { _ ~ updated }
  }
  object Expr {
    def apply[T](deps: EventSource*)(e: ⇒ T) = new Expr[T](Set(deps: _*))(e)
  }

  class DelayedExpr[T] private[Signals] (delay: Time)(depends: Set[EventSource])(expr: ⇒ T) extends Expr[T](depends)(expr) {
    override def delayed(moreDelay: Time) = new DelayedExpr(delay + moreDelay)(depends)(expr)
    override protected def updated {
      val v = expr
      after(delay) {
        updateValue(v)
      }
    }
  }

  class Socket[T] private () extends ActiveSignal[T] {
    private[this] var _source: Input[T] = Socket.unplugged
    def source = _source
    def source_=(src: Input[T]) = {
      _source = src
      src ~ { emit() }
    }
    final def :=(src: Input[T]) { source = src }
    def value = source.value
  }
  object Socket {
    private def unplugged[T] = ErrorSignal[T]("Unplugged socket")
    def apply[T](src: Input[T] = unplugged) = new Socket[T] { source = src }
  }
}
