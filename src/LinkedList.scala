import scala.annotation.tailrec

/**
  * Implements a functional singly-linked list
  */
object LinkedLists {

  sealed case class ::[+T](headItem: T, rest: LinkedList[T]) extends LinkedList[T]

  sealed abstract class LinkedList[+T] extends Ordered[LinkedList[_]] {

    final override lazy val toString: String = {
      @tailrec
      def represent[B >: T](accumulator: String, list: LinkedList[B], printType: Boolean): String = list match {
        case `[]` => if (!accumulator.isEmpty) accumulator else ""
        case x :: xs =>
          val typeInfo = if (printType) s"[${x.getClass.getName}] " else ""
          represent(s"""$accumulator${if (!accumulator.isEmpty) ", " else ""}$typeInfo$x""", xs, printType)
      }

      val globalTypeInfo = commonSuperclassOfElements.getName
      s"${getClass.getSuperclass.getName}[$globalTypeInfo](${represent("", this, printType = false)})"
    }
    final lazy val length: Int = {
      @tailrec
      def calcSize[B >: T](accumulator: Int, list: LinkedList[B]): Int = list match {
        case `[]` => accumulator
        case _ :: xs => calcSize(accumulator + 1, xs)
      }

      calcSize(0, this)
    }
    final lazy val unary_~ : LinkedList[T] = reversed
    final lazy val reversed: LinkedList[T] = createReversedList(0, `[]`)
    final lazy val /- : LinkedList[T] = lastDeleted
    final lazy val lastDeleted: LinkedList[T] = this - (length - 1)
    final lazy val \- : LinkedList[T] = firstDeleted
    final lazy val firstDeleted: LinkedList[T] = this - 0
    final lazy val head: Option[T] = first
    final lazy val first: Option[T] = this (0)

    /*override def equals(obj: Any): Boolean = obj match {
      case otherList: LinkedList[_] => (otherList eq this) || measureEquality(otherList)
      case _ => false
    }*/
    final lazy val last: Option[T] = this (length - 1)
    final private lazy val commonSuperclassOfElements: Class[_] = this match {
      case `[]` => classOf[Nothing]
      case other =>
        @tailrec
        def findCommonSuperclass(current: LinkedList[_], commonSuperclass: Class[_]): Class[_] = current match {
          case `[]` => commonSuperclass
          case x :: xs
            if x.getClass == commonSuperclass || x.getClass.getSuperclass == commonSuperclass =>
            findCommonSuperclass(xs, commonSuperclass)
          case _ :: xs => findCommonSuperclass(xs, commonSuperclass.getSuperclass)
        }

        findCommonSuperclass(other, other.head.get.getClass)
    }
    override lazy val hashCode: Int = toString hashCode

    final override def compare(that: LinkedList[_]): Int = toString compareTo (that toString)

    final def :::[B >: T](items: LinkedList[B]): LinkedList[B] = concatenated(items)

    final def concatenated[B >: T](Items: LinkedList[B]): LinkedList[B] = {
      @tailrec
      def createList(idx: Int, list: LinkedList[B]): LinkedList[B] = idx match {
        case Items.length => list
        case _ => createList(idx + 1, unwrapAndApply(Items(idx), list)(cons))
      }

      createList(0, this)
    }

    final private def unwrapAndApply[A, B](item: Option[B], list: LinkedList[A])
                                          (func: (LinkedList[A], B) => LinkedList[A]) =
      item match {
        case None => list
        case Some(value) => func(list, value)
      }

    final private def cons[A](list: LinkedList[A], item: A) = item :: list

    final def ::[B >: T](item: B): LinkedList[B] = constructed(item)

    final def constructed[B >: T](item: B): LinkedList[B] = new ::(item, this)

    @tailrec
    final def measureEquality[B >: T](that: LinkedList[B]): Boolean = (this, that) match {
      case (`[]`, `[]`) => true
      case (theseX :: _, thoseX :: _) if theseX != thoseX => false
      case (_ :: theseXs, _ :: thoseXs) => theseXs measureEquality thoseXs
    }

    final def toList: List[T] = this match {
      case `[]` => Nil
      case other =>
        @tailrec
        def createList[A](current: LinkedList[A], accumulator: List[A]): List[A] = current match {
          case `[]` => accumulator
          case x :: xs => createList(xs, x :: accumulator)
        }

        createList(other, Nil) reverse
    }

    final def ^-[B >: T](item: B): LinkedList[T] = removedItemFirst(item)

    final def removedItemFirst[B >: T](item: B): LinkedList[T] = ^(item) match {
      case None => this
      case Some(idx) => this - idx
    }

    final def removedItemAll[B >: T](item: B): LinkedList[B] = {
      @tailrec
      def removeItems(idxList: LinkedList[Int], listAccumulator: LinkedList[B]): LinkedList[B] = idxList match {
        case `[]` => listAccumulator
        case idx :: rest => removeItems(rest, listAccumulator - idx)
      }

      removeItems(indexesOf(item), this)
    }

    final def +[B >: T](item: B, Index: Int): LinkedList[B] = addedAt(item, Index)

    final def addedAt[B >: T](item: B, Index: Int): LinkedList[B] = Index match {
      case i if Index < 0 || Index >= length => this
      case _ =>
        this match {
          case `[]` => `[]`
          case _ =>
            @tailrec
            def createList(idxAccumulator: Int, listAccumulator: LinkedList[B]): LinkedList[B] = idxAccumulator match {
              case -1 => listAccumulator
              case Index => createList(idxAccumulator - 1, item :: listAccumulator)
              case otherIdx => createList(idxAccumulator - 1, unwrapAndApply(this (otherIdx), listAccumulator)(cons))
            }

            createList(length - 1, `[]`)
        }
    }

    final def -(Index: Int): LinkedList[T] = deletedAt(Index)

    final def deletedAt(Index: Int): LinkedList[T] = Index match {
      case i if Index < 0 || Index >= length => this
      case _ =>
        this match {
          case `[]` => `[]`
          case _ =>
            @tailrec
            def createList(idxAccumulator: Int, listAccumulator: LinkedList[T]): LinkedList[T] = idxAccumulator match {
              case -1 => listAccumulator
              case Index => createList(idxAccumulator - 1, listAccumulator)
              case otherIdx => createList(idxAccumulator - 1, unwrapAndApply(this (otherIdx), listAccumulator)(cons))
            }

            createList(length - 1, `[]`)
        }
    }

    final def ^[B >: T](item: B): Option[Int] = indexOf(item)

    final def indexOf[B >: T](item: B): Option[Int] = indexesOf(item).first

    final def lastIndexOf[B >: T](item: B): Option[Int] = indexesOf(item).last

    final def indexesOf[B >: T](item: B): LinkedList[Int] = this match {
      case `[]` => `[]`
      case other =>
        @tailrec
        def search(accumlator: Int, list: LinkedList[B], resultAccumulator: LinkedList[Int]): LinkedList[Int] = list match {
          case `[]` => resultAccumulator
          case x :: xs if x == item => search(accumlator + 1, xs, accumlator :: resultAccumulator)
          case _ :: xs => search(accumlator + 1, xs, resultAccumulator)
        }

        ~search(0, other, `[]`)
    }

    final def :+[B >: T](item: B): LinkedList[B] = appended(item)

    final def appended[B >: T](item: B): LinkedList[B] = ~new ::(item, ~this)

    final def apply(idx: Int): Option[T] = if (idx < 0 || idx >= length) None else
      this match {
        case `[]` => None
        case other =>
          @tailrec
          def search(accumlator: Int, list: LinkedList[T]): Option[T] = list match {
            case `[]` => None
            case x :: _ if accumlator == idx => Some(x)
            case _ :: xs => search(accumlator + 1, xs);
          }

          search(0, other)

      }

    @tailrec
    final private def createReversedList[B >: T](index: Int, accumulator: LinkedList[B]): LinkedList[B] = index match {
      case `length` => accumulator
      case other => createReversedList(other + 1, unwrapAndApply(this (other), accumulator)(cons))
    }
  }

  case object `[]` extends LinkedList[Nothing]

}

/**
  * Companion object to the [[LinkedLists.LinkedList]].
  *
  * Has some utility functions to convert between Scala collection types and native representation
  */
object LinkedList {

  import LinkedLists.{LinkedList, `[]`}

  def apply[T](items: T*): LinkedList[T] = {
    if (items.isEmpty) `[]` else {
      @tailrec
      def createList(index: Int, accumulator: LinkedList[T]): LinkedList[T] = index match {
        case -1 => accumulator
        case other => createList(other - 1, items(other) :: accumulator)
      }

      createList(items.length - 1, `[]`)
    }
  }

  def fromList[T](list: List[T]): LinkedList[T] = list match {
    case Nil => `[]`
    case other =>
      @tailrec
      def createList(current: List[T], accumulator: LinkedList[T]): LinkedList[T] = current match {
        case Nil => accumulator
        case x :: xs => createList(xs, x :: accumulator)
      }

      createList(other, `[]`)
  }
}

object FunctionalDataStructures {

  import LinkedLists.LinkedList

  /**
    * Implements a functional stack
    */
  type Stack[T] = LinkedList[T]

  /**
    * Implements a functional double-ended queue
    */
  type DEQueue[T] = LinkedList[T]

  object Pipes {

    class Pipe[A](a: A) {
      def |>[B](f: A => B): B = f(a)
    }

    object Pipe {
      def apply[A](v: A): Pipe[A] = new Pipe(v)
    }

    object PipeOps {
      implicit def toPipe[A](a: A): Pipe[A] = Pipe(a)
    }

  }

  object Stack {

    def apply[T](items: T*): Stack[T] = LinkedList.fromList(items.toList)

    def pop[T](stack: Stack[T]): Stack[T] = stack \-

    def dup[T](stack: Stack[T]): Stack[T] = peek(stack) match {
      case None => stack
      case Some(x) => push(stack)(x)
    }

    def push[T](stack: Stack[T])(item: T): Stack[T] = item :: stack

    def peek[T](stack: Stack[T]): Option[T] = stack head
  }

  object DEQueue {

    def apply[T](items: T*): DEQueue[T] = LinkedList.fromList(items.toList)

    def enqueueFront[T](dequeue: DEQueue[T])(item: T): DEQueue[T] = item :: dequeue

    def enqueue[T](dequeue: DEQueue[T])(item: T): DEQueue[T] = enqueueEnd(dequeue)(item)

    def enqueueEnd[T](dequeue: DEQueue[T])(item: T): DEQueue[T] = dequeue :+ item

    def dequeueEnd[T](dequeue: DEQueue[T]): DEQueue[T] = dequeue /-

    def peekEnd[T](dequeue: DEQueue[T]): (Option[T]) = dequeue last

    final def dequeue[T](dequeue: DEQueue[T]): DEQueue[T] = dequeueFront(dequeue)

    def dequeueFront[T](dequeue: DEQueue[T]): DEQueue[T] = dequeue \-

    def peek[T](dequeue: DEQueue[T]): (Option[T]) = peekFront(dequeue)

    def peekFront[T](dequeue: DEQueue[T]): (Option[T]) = dequeue head
  }

}

object Main {

  import LinkedLists._

  import io.StdIn.{readChar => rc, readLine => rl}

  def demoFunction(op: Char, item: String, linkedList: LinkedList[String]): Unit = {
    op match {
      case '+' => val newList = item :: linkedList; println(newList.toString + " " + newList.toList.toString); demoFunction(rc, rl, newList)
      case '-' => val newList = linkedList ^- item; println(newList); demoFunction(rc, rl, newList)
      case '^' => val newList = linkedList ^ item; println(newList); demoFunction(rc, rl, linkedList)
      case _ => println(linkedList)
    }
    assert(linkedList.toString.hashCode == linkedList.hashCode)
  }

  def main(args: Array[String]): Unit = {
    println(
      """Usage:
        |1. Add: + <Value>
        |2. Delete: - <Value>
        |3. Search: ^ <Value>""".stripMargin)
    demoFunction(io.StdIn.readChar(), io.StdIn.readLine(), `[]`)
  }
}