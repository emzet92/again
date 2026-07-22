package org.example

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  // toString
  def printElements: String

  override def toString: String = s"[$printElements]"

  def map[B](transformer: MyTransformer[A, B]): MyList[B]

  def flatMap[B](myTransformer: MyTransformer[A, MyList[B]]): MyList[B]

  def filter(predicate: MyPredicate[A]): MyList[A]
}

object Empty extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException()

  override def tail: MyList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = Cons(element, Empty)

  override def printElements: String = ""

  override def map[B](transformer: MyTransformer[Nothing, B]): MyList[Nothing] = Empty

  override def flatMap[B](myTransformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
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

  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = new Cons(transformer.transform(h), t.map(transformer))

  override def flatMap[B](myTransformer: MyTransformer[A, MyList[B]]): MyList[B] = ??? // TODO: implement concat first

  override def filter(predicate: MyPredicate[A]): MyList[A] = {
    if(predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }
}

trait MyPredicate[-T] {
  def test(cond: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(input: A): B
}


@main
def main(args: String*): Unit = {
  val list = Empty.add(1).add(2).add(3).filter((cond: Int) => cond > 2)
  println(list)
}