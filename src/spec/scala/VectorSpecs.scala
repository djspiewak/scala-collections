import org.specs._
import org.scalacheck._

import com.codecommit.collection.{EmptyVector, Vector}

object VectorSpecs extends Specification with ScalaCheck {
  import Prop._
  
  val vector = Vector[Int]()
  
  implicit def arbitraryVector[A](implicit arb: Arbitrary[A]): Arbitrary[Vector[A]] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[A]]
    } yield data.foldLeft(Vector[A]()) { _ + _ })
  }
  
  "vector" should {
    "store a single element" in {
      val prop = forAll { (i: Int, e: Int) =>
        i >= 0 ==> ((vector(0) = e)(0) == e)
      }
      
      prop must pass
    }
    
    "implement length" in {
      val prop = forAll { (list: List[Int]) => 
        val vec = list.foldLeft(Vector[Int]()) { _ + _ }
        vec.length == list.length
      }
      
      prop must pass
    }
    
    "replace single element" in {
      val prop = forAll { (vec: Vector[Int], i: Int) =>
        ((0 to vec.length) contains i) ==> {
          val newVector = (vec(i) = "test")(i) = "newTest"
          newVector(i) == "newTest"
        }
      }
      
      prop must pass
    }
    
    "fail on apply out-of-bounds" in {
      val prop = forAll { (vec: Vector[Int], i: Int) =>
        !((0 until vec.length) contains i) ==> {
          try {
            vec(i)
            false
          } catch {
            case _: IndexOutOfBoundsException => true
          }
        }
      }
      
      prop must pass
    }
    
    "fail on update out-of-bounds" in {
      val prop = forAll { (vec: Vector[Int], i: Int) =>
        !((0 to vec.length) contains i) ==> {
          try {
            vec(i) = 42
            false
          } catch {
            case _: IndexOutOfBoundsException => true
          }
        }
      }
      
      prop must pass
    }
    
    "pop elements" in {
      val prop = forAll { vec: Vector[Int] =>
        vec.length > 0 ==> {
          val popped = vec.pop
          var back = popped.length == vec.length - 1
          
          for (i <- 0 until popped.length) {
            back &&= popped(i) == vec(i)
          }
          
          back
        }
      }
      
      prop must pass(set(maxSize -> 3000, minTestsOk -> 1000))
    }
    
    "fail on pop empty vector" in {
      val caughtExcept = try {
        EmptyVector.pop
        false
      } catch {
        case _: IllegalStateException => true
      }
      
      caughtExcept mustEqual true
    }
    
    "store multiple elements in order" in {
      val prop = forAll { list: List[Int] =>
        val newVector = list.foldLeft(vector) { _ + _ }
        val res = for (i <- 0 until list.length) yield newVector(i) == list(i)
        
        res forall { _ == true }
      }
      
      prop must pass
    }
    
    "store lots of elements" in {
      val LENGTH = 100000
      val vector = (0 until LENGTH).foldLeft(Vector[Int]()) { _ + _ }
      
      vector.length mustEqual LENGTH
      for (i <- 0 until LENGTH) {
        vector(i) mustEqual i
      }
    }
    
    "implement filter" in {
      val prop = forAll { (vec: Vector[Int], f: (Int)=>Boolean) =>
        val filtered = vec filter f
        
        var back = filtered forall f
        for (e <- vec) {
          if (f(e)) {
            back &&= filtered.contains(e)
          }
        }
        back
      }
      
      prop must pass
    }
    
    "implement foldLeft" in {
      val prop = forAll { list: List[Int] =>
        val vec = list.foldLeft(Vector[Int]()) { _ + _ }
        vec.foldLeft(0) { _ + _ } == list.foldLeft(0) { _ + _ }
      }
      
      prop must pass
    }
    
    "implement forall" in {
      val prop = forAll { (vec: Vector[Int], f: (Int)=>Boolean) =>
        val bool = vec forall f
        
        var back = true
        for (e <- vec) {
          back &&= f(e)
        }
        
        (back && bool) || (!back && !bool)
      }
      
      prop must pass
    }
    
    "implement flatMap" in {
      val prop = forAll { (vec: Vector[Int], f: (Int)=>Vector[Int]) =>
        val mapped = vec flatMap f
        
        var back = true
        
        var i = 0
        var n = 0
        
        while (i < vec.length) {
          val res = f(vec(i))
          
          var inner = 0
          while (inner < res.length) {
            back &&= mapped(n) == res(inner)
            
            inner += 1
            n += 1
          }
          
          i += 1
        }
        
        back
      }
      
      prop must pass
    }
    
    "implement map" in {
      val prop = forAll { (vec: Vector[Int], f: (Int)=>Int) =>
        val mapped = vec map f
        
        var back = vec.length == mapped.length
        for (i <- 0 until vec.length) {
          back &&= mapped(i) == f(vec(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement reverse" in {
      val prop = forAll { v: Vector[Int] =>
        val reversed = v.reverse
        
        var back = v.length == reversed.length
        for (i <- 0 until v.length) {
          back &&= reversed(i) == v(v.length - i - 1)
        }
        back
      }
      
      prop must pass
    }
    
    "append to reverse" in {
      val prop = forAll { (v: Vector[Int], n: Int) =>
        val rev = v.reverse
        val add = rev + n
        
        var back = add.length == rev.length + 1
        for (i <- 0 until rev.length) {
          back &&= add(i) == rev(i)
        }
        back && add(rev.length) == n
      }
      
      prop must pass
    }
    
    "map on reverse" in {
      val prop = forAll { (v: Vector[Int], f: (Int)=>Int) =>
        val rev = v.reverse
        val mapped = rev map f
        
        var back = mapped.length == rev.length
        for (i <- 0 until rev.length) {
          back &&= mapped(i) == f(rev(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement subseq" in {
      val prop = forAll { (v: Vector[Int], from: Int, end: Int) =>
        try {
          val sub = v.subseq(from, end)
          
          var back = sub.length == end - from
          
          for (i <- 0 until sub.length) {
            back &&= sub(i) == v(i + from)
          }
          
          back
        } catch {
          case _:IndexOutOfBoundsException => from < 0 || end >= v.length
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "append to subseq" in {
      val prop = forAll { (v: Vector[Int], from: Int, end: Int, n: Int) =>
        try {
          val sub = v.subseq(from, end)
          val add = sub + n
          
          var back = add.length == sub.length + 1
          for (i <- 0 until sub.length) {
            back &&= add(i) == sub(i)
          }
          back && add(sub.length) == n
        } catch {
          case _:IndexOutOfBoundsException => from < 0 || end >= v.length
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "update subseq" in {
      val prop = forAll { (v: Vector[Int], from: Int, end: Int, mi: Int) =>
        try {
          val sub = v.subseq(from, end)
          val add = sub(mi) = 42
          
          var back = add.length == sub.length + (if (mi == sub.length) 1 else 0)
          for (i <- 0 until sub.length; if i != mi) {
            back &&= add(i) == sub(i)
          }
          back && add(mi) == 42
        } catch {
          case _:IndexOutOfBoundsException => from < 0 || end >= v.length || !(0 to (end - from) contains mi)
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "map on subseq" in {
      val prop = forAll { (v: Vector[Int], from: Int, end: Int, f: (Int)=>Int) =>
        try {
          val sub = v.subseq(from, end)
          val mapped = sub map f
          
          var back = mapped.length == sub.length
          for (i <- 0 until sub.length) {
            back &&= mapped(i) == f(sub(i))
          }
          back
        } catch {
          case _:IndexOutOfBoundsException => from < 0 || end >= v.length
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "implement zip" in {
      val prop = forAll { (first: Vector[Int], second: Vector[Double]) =>
        val zip = first zip second
        
        var back = zip.length == Math.min(first.length, second.length)
        for (i <- 0 until zip.length) {
          var (left, right) = zip(i)
          back &&= (left == first(i) && right == second(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement zipWithIndex" in {
      val prop = forAll { vec: Vector[Int] =>
        val zip = vec.zipWithIndex
        
        var back = zip.length == vec.length
        for (i <- 0 until zip.length) {
          val (elem, index) = zip(i)
          
          back &&= (index == i && elem == vec(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement equals" in {
      {
        val prop = forAll { list: List[Int] => 
          val vecA = list.foldLeft(Vector[Int]()) { _ + _ }
          val vecB = list.foldLeft(Vector[Int]()) { _ + _ }
          
          vecA == vecB
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (vecA: Vector[Int], vecB: Vector[Int]) =>
          vecA.length != vecB.length ==> (vecA != vecB)
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (listA: List[Int], listB: List[Int]) =>
          val vecA = listA.foldLeft(Vector[Int]()) { _ + _ }
          val vecB = listB.foldLeft(Vector[Int]()) { _ + _ }
          
          listA != listB ==> (vecA != vecB)
        }
        
        prop must pass
      }
      
      {
        val prop = forAll { (vec: Vector[Int], data: Int) => vec != data }
        
        prop must pass
      }
    }
    
    "implement hashCode" in {
      val prop = forAll { list: List[Int] =>
        val vecA = list.foldLeft(Vector[Int]()) { _ + _ }
        val vecB = list.foldLeft(Vector[Int]()) { _ + _ }
        
        vecA.hashCode == vecB.hashCode
      }
      
      prop must pass
    }
    
    "implement extractor" in {
      val vec1 = Vector(1, 2, 3)
      vec1 must beLike {
        case Vector(a, b, c) => (a, b, c) == (1, 2, 3)
      }
      
      val vec2 = Vector("daniel", "chris", "joseph")
      vec2 must beLike {
        case Vector(a, b, c) => (a, b, c) == ("daniel", "chris", "joseph")
      }
    }
  }
}

