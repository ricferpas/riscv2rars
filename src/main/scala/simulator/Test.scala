package simulator

import scala.collection._
import mutable.PriorityQueue

class Test1 extends App with Simulator with Signals {
  def suma(a: Signal[Int], b: Signal[Int]) = Expr(a, b) { a() + b() }

  val a1 = Var(4)
  val a2s = Var(5)
  val a2 = a2s delayed 1 delayed 1

  val c1 = Expr(a1, a2) { a1() - a2() }

  val b1 = Expr(a1, a2) { a1() + a2() }
  val b2 = suma(a1, a2) delayed 8

  a1 ~ { println(f"${currentTime}%5d a1: ${a1()}") }
  a2 ~ { println(f"${currentTime}%5d a2: ${a2()}") }
  a2s ~ { println(f"${currentTime}%5d a2s: ${a2s()}") }
  b1 ~ { println(f"${currentTime}%5d b1: ${b1()}") }
  b2 ~ { println(f"${currentTime}%5d b2: ${b2()}") }
  c1 ~ { println(f"${currentTime}%5d c1: ${c1()}") }

  after(10) { a1.value = 3 }
  after(5) { a2s.value = 6 }

  def debug {
    println(f"${currentTime}%5d ${a1()}%5d ${a2s()}%5d ${a2()}%5d ${c1()}%5d ${b1()}%5d ${b2()}%5d ")
    if (currentTime < 20)
      after(1) { debug }
  }

  after(0) { debug }
  run()
}

class Test2 extends Logic with App {
  val s = Switch(false)
  val r = Switch(false)
  val norqn = Nor(s.out, ??)
  val norq = Nor(r.out, norqn.out)
  norqn.y := norq.out
  val q = norq.out
  val qn = norqn.out

  after(10) { s.state := true }
  after(21) { s.state := false }

  after(37) { r.state := true }
  after(42) { r.state := false }

  after(137) { r.state := true }
  after(144) { r.state := false }

  def pt() = print(f"${currentTime}%5d ")
  s.state ~ { pt(); println("s:" + s.out()) }
  r.state ~ { pt(); println("r:" + r.out()) }
  q ~ { pt(); println("q:" + q()) }
  qn ~ { pt(); println("qn:" + qn()) }

  run()
}

object Test extends Test2
