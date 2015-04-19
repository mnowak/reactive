package quickcheck

import java.lang.Math.min

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val h2 = insert(b, insert(a, empty))
    findMin(h2) == min(a, b)
  }

  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("hint3") = forAll { (h: H) =>
    isSorted(asSortedList(h))
  }

  def asSortedList(h: H): List[A] = asSortedListAgg(List(findMin(h)), deleteMin(h))

  def asSortedListAgg(list: List[A], h: H): List[A] = {
    if (isEmpty(h)) list
    else asSortedListAgg(list :+ findMin(h), deleteMin(h))
  }

  def isSorted(list: List[A]): Boolean = {
    if (list.isEmpty) true
    else isSortedAgg(list.head, list.tail)

  }

  def isSortedAgg(a: A, list: List[A]): Boolean = {
    if (list.isEmpty) true
    else a <= list.head && isSortedAgg(list.head, list.tail)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("meld empty") = forAll{ (h:H) =>
    heapsEqual(h, meld(h, empty))
    heapsEqual(h, meld(empty, h))
  }

  // result of melding of two heaps should be equal to melding of heaps with one element transferred from one to another
  property("meld") = forAll { (h1: H, h2: H) =>
    heapsEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  def heapsEqual(h1: H, h2: H): Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val a = findMin(h1)
      val b = findMin(h2)
      a == b && heapsEqual(deleteMin(h1), deleteMin(h2))
    }
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
