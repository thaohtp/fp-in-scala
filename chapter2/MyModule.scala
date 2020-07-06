object MyModule {
  def abs(x: Int) : Int = if(x < 0) -x else x
  
  def factorial(x: Int): Int = {
    @annotation.tailrec // this annotation validates if this is a tail recursive func.
    def calc(x: Int, currentFact: Int): Int = {
      if(x <=1) 
        currentFact 
      else
        calc(x-1, currentFact * x)
    }
    calc(x, 1)
  }

  def fibonaci(x: Int): Int = {
    @annotation.tailrec
    def calc(x: Int, fibonaci_minus_1: Int, fibonaci_minus_2: Int): Int = {
      if (x == 0)
        fibonaci_minus_2
      else if(x == 1)
        fibonaci_minus_1 
      else 
        calc(x - 1, fibonaci_minus_1 + fibonaci_minus_2, fibonaci_minus_1)
    }

    calc(x, 1, 0)
  }
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def calc(index: Int): Boolean = {
      if(index == as.length - 1)
        true
      else if(!ordered(as(index), as(index+1)))
        false
      else
        calc(index + 1)
    }

    calc(0)
  }

  def formatResult(funcName: String, n: Int, func: Int => Int): String = 
    s"The $funcName value of $n is ${func(n)}."

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => (b => f(a,b))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      val g = f(a)
      (a, b) => g(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]) : Unit = {
    println(formatResult("abs", -1, abs))
    println("--------------------------------------------------------------")
    println(s"Test fibonaci 0 is ${fibonaci(0)}: ${fibonaci(0) == 0}")
    println(s"Test fibonaci 1 is ${fibonaci(1)}: ${fibonaci(1) == 1}")
    println(s"Test fibonaci 2 is ${fibonaci(2)}: ${fibonaci(2) == 1}")
    println(s"Test fibonaci 3 is ${fibonaci(3)}: ${fibonaci(3) == 2}")
    println(s"Test fibonaci 4 is ${fibonaci(4)}: ${fibonaci(4) == 3}")
    println(s"Test fibonaci 5 is ${fibonaci(5)}: ${fibonaci(5) == 5}")
    println(s"Test fibonaci 6 is ${fibonaci(6)}: ${fibonaci(6) == 8}")

    println("--------------------------------------------------------------")

    println(s"isSorted: ${(isSorted(Array(1, 2, 2, 3), (a: Int, b:Int) => a <= b))}")
    println(s"isSorted: ${(isSorted(Array(1, 2, 5, 3), (a: Int, b:Int) => a <= b))}")
  }
}
