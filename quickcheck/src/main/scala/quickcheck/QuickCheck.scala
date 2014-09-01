package quickcheck

import common._

import scala.annotation.tailrec
import scala.math._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == min(a, b)
  }

  property("one element heap should be empty after deleting the min") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert ascending values") = forAll { (pair: Pair[Int, Int]) =>
    val h = insert(pair._2, insert(pair._1, empty))
    (findMin(h) == pair._1) && (findMin(deleteMin(h)) == pair._2)
  }

  /**
   * Transform the heap into a descending sorted list
   */
  @tailrec
  private def heapToList(elems: List[A], heap: H): List[A] = {
    if (isEmpty(heap)) elems
    else heapToList(findMin(heap) :: elems, deleteMin(heap))
  }

  property("the heap should return a sorted sequence") = forAll { h: H =>
    val heapElems = heapToList(List(), h)
    if (heapElems.isEmpty) true
    else (heapElems, heapElems.tail).zipped.forall(ord.gteq(_ , _))
  }

  property("min of melding heaps") = forAll { (h1: H, h2: H) =>
    val minMeld = findMin(meld(h1, h2))
    (minMeld == findMin(h1) || minMeld == findMin(h2))
  }

  property("length of melding heap should be the sum of merged heaps") = forAll { (h1: H, h2: H) =>
    @tailrec
    def heapLength(h: H, length: Int): Int =
      if (isEmpty(h)) length
      else heapLength(deleteMin(h), length + 1)
    heapLength(meld(h1, h2), 0) == (heapLength(h1, 0) + heapLength(h2, 0))
  }

  property("input list is equal with output") = forAll { l: List[Int] =>
    val heap = l.sorted.foldLeft(empty)((h, e) => insert(e, h))
    val heapList = heapToList(List(), heap).reverse
    l.sorted == heapList
  }

  /**
   * Generate random heaps
   */
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
   * Generate ascending pair
   */
  lazy val genIncreasePair: Gen[Pair[Int, Int]] = for {
    x <- Gen.choose(0, 100)
    y <- x + 1
  } yield Pair(x, y)

  implicit lazy val arbIncreasePair: Arbitrary[Pair[Int, Int]] = Arbitrary(genIncreasePair)
}