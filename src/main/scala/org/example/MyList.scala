package org.example

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  // toString
  def printElements: String

  override def toString: String = s"[$printElements]"
}

object Empty extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException()

  override def tail: MyList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = Cons(element, Empty)

  override def printElements: String = ""
}

class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = Cons(element, this)

  override def printElements: String = {
    if (t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  }
}


@main
def main(args: String*): Unit = {
  val list = Empty.add(1).add(2).add(3)
  println(list.toString)
}