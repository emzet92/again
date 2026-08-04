package org.example

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B]

  // toString
  def printElements: String

  override def toString: String = s"[$printElements]"

  def map[B](transformer: A => B): MyList[B]

  def flatMap[B](myTransformer: A => MyList[B]): MyList[B]

  def filter(predicate: A => Boolean): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]
}

case object Empty extends MyList[Nothing] {

  override def head: Nothing = throw new NoSuchElementException()

  override def tail: MyList[Nothing] = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = Cons(element, Empty)

  override def printElements: String = ""

  override def map[B](transformer: Nothing => B): MyList[Nothing] = Empty

  override def flatMap[B](myTransformer: Nothing => MyList[B]): MyList[B] = Empty

  override def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  override def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = Cons(element, this)

  override def printElements: String = {
    if (t.isEmpty) "" + h
    else s"$h ${t.printElements}"
  }

  override def map[B](transformer: A=>B): MyList[B] = new Cons(transformer(h), t.map(transformer))


  /*
   * Przykład działania:
   *
   * Lista:
   * Cons(1, Cons(2, Cons(3, Empty)))
   *
   * Transformer:
   * 1 => Cons(1, Cons(1, Empty))
   * 2 => Cons(2, Cons(2, Empty))
   * 3 => Cons(3, Cons(3, Empty))
   *
   * Wywołanie:
   *
   * Cons(1, Cons(2, Cons(3, Empty))).flatMap(transformer)
   *
   * = transformer.transform(1) ++
   *   Cons(2, Cons(3, Empty)).flatMap(transformer)
   *
   * = Cons(1, Cons(1, Empty)) ++
   *   (
   *     transformer.transform(2) ++
   *     Cons(3, Empty).flatMap(transformer)
   *   )
   *
   * = Cons(1, Cons(1, Empty)) ++
   *   (
   *     Cons(2, Cons(2, Empty)) ++
   *     (
   *       transformer.transform(3) ++
   *       Empty.flatMap(transformer)
   *     )
   *   )
   *
   * = Cons(1, Cons(1, Empty)) ++
   *   (
   *     Cons(2, Cons(2, Empty)) ++
   *     (
   *       Cons(3, Cons(3, Empty)) ++
   *       Empty
   *     )
   *   )
   *
   * Dla Empty:
   *
   * Empty.flatMap(transformer) = Empty
   *
   * Po zwinięciu rekurencji:
   *
   * Cons(
   *   1,
   *   Cons(
   *     1,
   *     Cons(
   *       2,
   *       Cons(
   *         2,
   *         Cons(
   *           3,
   *           Cons(3, Empty)
   *         )
   *       )
   *     )
   *   )
   * )
   *
   * Wynik:
   *
   * [1, 1, 2, 2, 3, 3]
   *
   * Metoda wykonuje transformację dla elementu head.
   * Transformer zwraca nową listę, która jest następnie łączona
   * operatorem ++ z wynikiem flatMap dla pozostałej części listy.
   *
   * Rekurencja kończy się po dotarciu do Empty.
   */
  override def flatMap[B](
                           myTransformer: A => MyList[B]
                         ): MyList[B] =
    myTransformer(head) ++ t.flatMap(myTransformer)


  override def filter(predicate: A => Boolean): MyList[A] = {
    if (predicate(h)) Cons(h, t.filter(predicate))
    else t.filter(predicate)
  }


  /*
   * Przykład działania:
   *
   * Cons(1, Cons(2, Cons(3, Empty))) ++ Cons(4, Cons(5, Empty))
   *
   * = Cons(1, Cons(2, Cons(3, Empty)) ++ Cons(4, Cons(5, Empty)))
   * = Cons(1, Cons(2, Cons(3, Empty) ++ Cons(4, Cons(5, Empty))))
   * = Cons(1, Cons(2, Cons(3, Empty ++ Cons(4, Cons(5, Empty)))))
   *
   * Dla Empty:
   * Empty ++ list = list
   *
   * Po zwinięciu rekurencji:
   * Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Empty)))))
   *
   * Metoda przechodzi przez wszystkie elementy pierwszej listy,
   * odtwarza je w nowej liście, a na jej końcu dołącza drugą listę.
   *
   * Złożoność czasowa: O(n),
   * gdzie n oznacza długość pierwszej listy.
   */
  override def ++[B >: A](list: MyList[B]): MyList[B] =
    new Cons(h, t ++ list)

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
  val list2 = Empty.add(22).add(33).add(44)

  println(list)
  println(list ++ list2)
  println(list.flatMap((input: Int) => list ++ list2.map(_ * 2)))

  // alternative using for comprehension
  println(
    for {
      input <- list
      element <- list ++ list2.map(n => n * 2)
    } yield element
  )
}