package tasks.adts

import u03.extensionmethods.Sequences.{Sequence, *}
import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.extensionmethods.Sequences.Sequence.{Cons, nil}

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional if stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]

    def empty[A]: Stack[A] // factory

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]

    def empty[A]: Stack[A] = nil()

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = Cons(a, stack)
      def pop(): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) if h != nil() => Just(h, t)
        case _ => Optional.Empty()
      def asSequence(): Sequence[A] = stack match
        case Cons(h, t) => stack
        case _ => nil()