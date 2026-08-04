package org.example.functions

// NOTE: Apply is special
trait MyFunction[A, B] {
  def apply(element: A): B
}

val doubler: Int => Int = (element: Int) => element * 2

val concatenator: (String, String) => String = (a, b) => a + b

@main
def main(args: String*): Unit = {
  println(doubler(2))
  println(concatenator("a", "b"))
}