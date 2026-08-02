package org.example
import scala.language.postfixOps

def sumTo(n: Int): Int = if (n == 0) {
  0
} else {
  n + sumTo(n - 1)
}

extension (n: Int)
  def  sumItself: Int = sumTo(n)

@main
def app(args: String*): Unit = {
  println("nice")
  println(sumTo(5))
  println(5 sumItself)
}
