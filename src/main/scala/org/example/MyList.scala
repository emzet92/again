package org.example

abstract class MyList {
  def head: Int

  def tail: MyList

  def isEmpty: Boolean

  def add(element: Int): MyList

  // toString
  def printElements: String

  override def toString: String = s"[$printElements]"
}

object Empty extends MyList {

  override def head: Int = throw new NoSuchElementException()

  override def tail: MyList = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def add(element: Int): MyList = Cons(element, Empty)

  override def printElements: String = ""
}

class Cons(h: Int, t: MyList) extends MyList {

  override def head: Int = h

  override def tail: MyList = t

  override def isEmpty: Boolean = false

  override def add(element: Int): MyList = Cons(element, this)

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