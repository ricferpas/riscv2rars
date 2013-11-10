package simulator

object tests extends Simulator with Signals {
  def suma(a: Signal[Int], b: Signal[Int]) = Expr(a, b) { a() + b() }
                                                  //> suma: (a: simulator.tests.Signal[Int], b: simulator.tests.Signal[Int])simula
                                                  //| tor.tests.Expr[Int]

  val a1 = Var(4)                                 //> a1  : simulator.tests.Var[Int] = simulator.Signals$Var@3595ecae
  val a2s = Var(5)                                //> a2s  : simulator.tests.Var[Int] = simulator.Signals$Var@287cbe54
  val a2 = a2s delayed 1 delayed 1                //> a2  : simulator.tests.DelayedSignal[Int] = simulator.Signals$DelayedSignal@6
                                                  //| f8cc276
  val c1 = Expr(a1, a2) { a1() - a2() }           //> c1  : simulator.tests.Expr[Int] = simulator.Signals$Expr@685269ca

  val b1 = Expr(a1, a2) { a1() + a2() }           //> b1  : simulator.tests.Expr[Int] = simulator.Signals$Expr@7e224235
  val b2 = suma(a1, a2) delayed 8                 //> b2  : simulator.tests.DelayedSignal[Int] = simulator.Signals$DelayedSignal@2
                                                  //| b76be4
  a1 ~ { println(f"${currentTime}%5d a1: ${a1()}") }
  a2 ~ { println(f"${currentTime}%5d a2: ${a2()}") }
  b1 ~ { println(f"${currentTime}%5d b1: ${a1()} + ${a2()} = ${b1()}") }
  b2 ~ { println(f"${currentTime}%5d b2: ${a1()} + ${a2()} = ${b2()}") }
  c1 ~ { println(f"${currentTime}%5d c1: ${a1()} - ${a2()} = ${c1()}") }

  after(10) { a1.value = 3 }
  after(5) { a2s.value = 6 }

  def debug {
    println(f"${currentTime}%5d ${a1()}%5d ${a2s()}%5d ${a2()}%5d ${c1()}%5d ${b1()}%5d ${b2()}%5d ")
    if (currentTime < 20) after(1) { debug }
  }                                               //> debug: => Unit
  after(0) { debug }

  run()                                           //>     0 a1: 4
                                                  //|     0     4     5     0     4     4     0 
                                                  //|     0 b1: 4 + null = 4
                                                  //|     0 c1: 4 - null = 4
                                                  //|     1     4     5     0     4     4     0 
                                                  //|     2 a2: 5
                                                  //|     2 b1: 4 + 5 = 9
                                                  //|     2 c1: 4 - 5 = -1
                                                  //|     2     4     5     5    -1     9     0 
                                                  //|     3     4     5     5    -1     9     0 
                                                  //|     4     4     5     5    -1     9     0 
                                                  //|     5     4     6     5    -1     9     0 
                                                  //|     6     4     6     5    -1     9     0 
                                                  //|     7 a2: 6
                                                  //|     7 b1: 4 + 6 = 10
                                                  //|     7 c1: 4 - 6 = -2
                                                  //|     7     4     6     6    -2    10     0 
                                                  //|     8 b2: 4 + 6 = 4
                                                  //|     8     4     6     6    -2    10     4 
                                                  //|     9     4     6     6    -2    10     4 
                                                  //|    10 b2: 4 + 6 = 9
                                                  //|    10     4     6     6    -2    10     9 
                                                  //|    10 a1: 3
                                                  //|    10 b1: 3 + 6 = 9
                                                  //|    10 c1: 3 - 6 = -3
                                                  //|    11     3     6     6    -3     9     9 
                                                  //|    12     3     6     6    -3     9     9 
                                                  //|    13     3     6     6    -3     9     9 
                                                  //|    14     3     6     6 
                                                  //| Output exceeds cutoff limit.
}