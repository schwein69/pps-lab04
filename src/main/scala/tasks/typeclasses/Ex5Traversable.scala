package u04lab

import u03.Sequences.*
import Sequence.*
import scala.annotation.tailrec
import u03.Optionals.*, Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](t: A): Unit = println("The next element is: " + t)

  def logAll[T[_] : Traversable, A](t: T[A]): Unit =
    summon[Traversable[T]].foreach(t)(log)

  trait Traversable[T[_]]:
    def foreach[A](ta: T[A])(f: A => Unit): Unit

  given Traversable[Optional] with
    def foreach[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Just(value) => f(value)
      case Empty() => ()

  given Traversable[Sequence] with
    def foreach[A](opt: Sequence[A])(f: A => Unit): Unit = opt match
      case Cons(h, t) => f(h); foreach(t)(f)
      case Nil() => ()

  @main def tryTraversables =
    val optional = Just(5)
    log(optional)
    logAll(optional)
    val sequence = Cons(5, Cons(8, Cons(9, Nil())))
    log(sequence)
    logAll(sequence)


