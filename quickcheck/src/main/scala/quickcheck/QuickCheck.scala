package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{forAll, throws}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      x <- arbitrary[Int]
      h <- genHeap
    }yield {
      insert(x, h)
    }
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (i: Int) =>
    val h = insert(i, empty)
    findMin(h) == i
  }

  property("min2") = forAll { (x: Int, y: Int) =>
    val h = insert(y, insert(x, empty))
    findMin(h) == math.min(x, y)
  }

  property("del1") = forAll { (i: Int) =>
    val h = deleteMin(insert(i, empty))
    isEmpty(h)
  }

  def isSorted(h: H, prevDeleted: Option[Int] = None): Boolean = {
    if(isEmpty(h)){
      true
    }else{
      val currMin = findMin(h)
      prevDeleted.forall(_ <= currMin) && isSorted(deleteMin(h), Some(currMin))
    }
  }

  property("sortedFindDel") = forAll { (h: H) =>
    isSorted(h)
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    (isEmpty(h1), isEmpty(h2)) match {
      case (true, true) => isEmpty(meld(h1, h2))
      case (true, false) => findMin(meld(h1, h2)) == findMin(h2)
      case (false, true) => findMin(meld(h1, h2)) == findMin(h1)
      case (false, false) => findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
    }
  }

  property("meld2Sorted") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("delEmpty") = throws(classOf[Exception]){
    deleteMin(empty)
  }

  def isSubHeap(subHeapCandidate: H, heap: H): Boolean = (isEmpty(subHeapCandidate), isEmpty(heap)) match {
    case (true, _) => true
    case (false, true) => false
    case (false, false) =>
      val minOfSubHeap = findMin(subHeapCandidate)
      val minOfHeap = findMin(heap)
      if(minOfHeap < minOfSubHeap){
        isSubHeap(subHeapCandidate, deleteMin(heap))
      }else if(minOfHeap == minOfSubHeap){
        isSubHeap(deleteMin(subHeapCandidate), deleteMin(heap))
      }else{
        false
      }
  }

  property("meldedContained") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    isSubHeap(h1, melded) && isSubHeap(h2, melded)
  }


