package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      x <- arbitrary[A]
      h <- oneOf[H](empty, genHeap)
    } yield insert(x, h)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two") = forAll { (x: Int, y: Int) =>
    val result = insert(y, insert(x, empty))
    findMin(result) == (x min y)
  }

  property("insert and delete") = forAll {
    (x: Int) =>
      val result = insert(x, empty)
      isEmpty(deleteMin(result))
  }

  property("delete min") = forAll { (x: Int, y: Int) =>
    val result = insert(y, insert(x, empty))
    val max = x max y
    findMin(deleteMin(result)) == max
  }

  def insertAll(l: List[Int], heap: H): H = l match {
    case Nil => heap
    case x :: rest => insertAll(rest, insert(x, heap))
  }

  def deleteAll(l: List[Int], heap: H): List[Int] = {
    if (isEmpty(heap)) l
    else findMin(heap) :: deleteAll(l, deleteMin(heap))
  }

  def sorted(list: List[Int]): Boolean = list match {
    case Nil => true
    case x :: Nil => true
    case x :: rest => x <= rest.head && sorted(rest)
  }

  property("sorted") = forAll { (list: List[Int]) =>
    val result = insertAll(list, empty)
    val sortedList = deleteAll(List(), result)
    val input = list.sorted
    input == sortedList
  }

  property("meld") = forAll { (x: Int, y: Int) =>
    val h1 = insert(x, empty)
    val h2 = insert(y, empty)
    val melted = meld(h1, h2)
    findMin(melted) == (x min y)
  }

  property("meld empty") = {
    val melted = meld(empty, empty)
    isEmpty(melted)
  }

  property("meld with lists") = forAll { (x: List[Int], y: List[Int]) =>
    val h1 = insertAll(x, empty)
    val h2 = insertAll(y, empty)
    val melted = meld(h1, h2)
    if (isEmpty(melted)) x.isEmpty && y.isEmpty
    else if (x.isEmpty) findMin(melted) == y.min
    else if (y.isEmpty) findMin(melted) == x.min
    else findMin(melted) == (x.min min y.min)
  }

  property("meld sorted") = forAll { (x: List[Int], y: List[Int]) =>
    val h1 = insertAll(List(0), empty)
    val h2 = insertAll(List(0), empty)
    val melted = meld(h1, h2)
    val sortedList = deleteAll(List(), melted)
    sorted(sortedList)
  }

}
