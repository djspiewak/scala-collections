import org.specs._
import org.scalacheck._

import com.codecommit.collection.PathVector

object PathVectorSpecs extends Specification with Scalacheck {
  import Prop._
  
  val vector = PathVector[Int]()
  
  implicit def arbitraryPathVector[A](implicit arb: Arbitrary[A]): Arbitrary[PathVector[A]] = {
    Arbitrary(for {
      data <- Arbitrary.arbitrary[List[A]]
      indexes <- Gen.containerOfN[List, Int](data.length, Gen.choose(0, Math.MAX_INT - 1))
    } yield {
      var vec = new PathVector[A]
      
      var i = 0
      for (d <- data) {
        vec(indexes(i)) = d
        i += 1
      }
      
      vec
    })
  }
  
  
  "path vector" should {
    "have infinite bounds" in {
      vector.length mustEqual 0     // finite length
      
      val prop = property { i: Int =>   // infinite bounds
        i >= 0 ==> (vector(i) == 0)
      }
      
      prop must pass
    }
    
    "store a single element" in {
      val prop = property { (i: Int, e: Int) =>
        i >= 0 ==> ((vector(0) = e)(0) == e)
      }
      
      prop must pass
    }
    
    "replace single element" in {
      val prop = property { (vec: PathVector[String], i: Int) =>
        i >= 0 ==> {
          val newPathVector = (vec(i) = "test")(i) = "newTest"
          newPathVector(i) == "newTest"
        }
      }
      
      prop must pass
    }
    
    "store multiple elements in order" in {
      val prop = property { list: List[Int] =>
        val newPathVector = list.foldLeft(vector) { _ + _ }
        val res = for (i <- 0 until list.length) yield newPathVector(i) == list(i)
        
        res forall { _ == true }
      }
      
      prop must pass
    }
    
    "store lots of elements" in {
      val LENGTH = 100000
      val vector = (0 until LENGTH).foldLeft(PathVector[Int]()) { _ + _ }
      
      vector.length mustEqual LENGTH
      for (i <- 0 until LENGTH) {
        vector(i) mustEqual i
      }
    }
    
    "store at arbitrary points" in {
      val vector = PathVector(1, 2, 3, 4, 5)
      val prop = property { others: List[(Int, Int)] =>
        val (newPathVector, resMap) = others.foldLeft(vector, Map[Int, Int]()) { (inTuple, tuple) =>
          val (i, value) = tuple
          val (vec, map) = inTuple
          
          if (i < 0) (vec, map) else (vec(i) = value, map + (i -> value))
        }
        
        val res = for {
          (i, _) <- others
        } yield if (i < 0) true else newPathVector(i) == resMap(i)
        
        res forall { _ == true }
      }
      
      prop must pass
    }
    
    "implement filter" in {
      val prop = property { (vec: PathVector[Int], f: (Int)=>Boolean) =>
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
      val prop = property { list: List[Int] =>
        val vec = list.foldLeft(new PathVector[Int]) { _ + _ }
        vec.foldLeft(0) { _ + _ } == list.foldLeft(0) { _ + _ }
      }
      
      prop must pass
    }
    
    "implement forall" in {
      val prop = property { (vec: PathVector[Int], f: (Int)=>Boolean) =>
        val bool = vec forall f
        
        var back = true
        for (e <- vec) {
          back &&= f(e)
        }
        
        (back && bool) || (!back && !bool)
      }
      
      prop must pass
    }
    
    "implement map" in {
      val prop = property { (vec: PathVector[Int], f: (Int)=>Int) =>
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
      val prop = property { v: PathVector[Int] =>
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
      val prop = property { (v: PathVector[Int], n: Int) =>
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
      val prop = property { (v: PathVector[Int], f: (Int)=>Int) =>
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
      val prop = property { (v: PathVector[Int], from: Int, end: Int) =>
        try {
          val sub = v.subseq(from, end)
          
          var back = sub.length == end - from
          for (i <- 0 until sub.length) {
            back &&= sub(i) == v(i + from)
          }
          back
        } catch {
          case _:IndexOutOfBoundsException => from < 0
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "append to subseq" in {
      val prop = property { (v: PathVector[Int], from: Int, end: Int, n: Int) =>
        try {
          val sub = v.subseq(from, end)
          val add = sub + n
          
          var back = add.length == sub.length + 1
          for (i <- 0 until sub.length) {
            back &&= add(i) == sub(i)
          }
          back && add(sub.length) == n
        } catch {
          case _:IndexOutOfBoundsException => from < 0
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "map on subseq" in {
      val prop = property { (v: PathVector[Int], from: Int, end: Int, f: (Int)=>Int) =>
        try {
          val sub = v.subseq(from, end)
          val mapped = sub map f
          
          var back = mapped.length == sub.length
          for (i <- 0 until sub.length) {
            back &&= mapped(i) == f(sub(i))
          }
          back
        } catch {
          case _:IndexOutOfBoundsException => from < 0
          case _:IllegalArgumentException => end <= from
        }
      }
      
      prop must pass
    }
    
    "implement zip" in {
      val prop = property { (first: PathVector[Int], second: PathVector[Double]) =>
        val zip = first zip second
        
        var back = zip.length == Math.max(first.length, second.length)
        for (i <- 0 until zip.length) {
          var (left, right) = zip(i)
          back &&= (left == first(i) && right == second(i))
        }
        back
      }
      
      prop must pass
    }
    
    "implement zipWithIndex" in {
      val prop = property { vec: PathVector[Int] =>
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
      val prop = property { list: List[Int] => 
        val vecA = list.foldLeft(new PathVector[Int]) { _ + _ }
        val vecB = list.foldLeft(new PathVector[Int]) { _ + _ }
        
        vecA == vecB
      }
      
      prop must pass
    }
  }
}
