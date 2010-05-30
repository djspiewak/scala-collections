import org.specs._
import org.scalacheck._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import com.codecommit.collection.BloomSet

object BloomSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "bloom set" should {
    "store single element once" in {
      (BloomSet[String]() + "test") contains "test" mustEqual true
    }
    
    "store single element n times" in {
      val prop = forAll { ls: List[String] =>
        val set = ls.foldLeft(BloomSet[String]()) { _ + _ }
        
        ls.foldLeft(true) { _ && set(_) }
      }
      
      prop must pass
    }
    
    "store duplicate elements n times" in {
      val prop = forAll { ls: List[String] =>
        var set = ls.foldLeft(BloomSet[String]()) { _ + _ }
        set = ls.foldLeft(set) { _ + _ }
        
        ls.foldLeft(true) { _ && set(_) }
      }
      
      prop must pass
    }
    
    "handle ++ Iterable" in {
      val prop = forAll { (first: List[String], last: List[String]) =>
        var set = first.foldLeft(BloomSet[String]()) { _ + _ }
        set ++= last
        
        first.foldLeft(true) { _ && set(_) } && last.foldLeft(true) { _ && set(_) }
      }
      
      prop must pass
    }
    
    "handle ++ BloomSet" in {
      val prop = forAll { (first: List[String], last: List[String]) =>
        var set = first.foldLeft(BloomSet[String]()) { _ + _ }
        set ++= last.foldLeft(BloomSet[String]()) { _ + _ }
        
        first.foldLeft(true) { _ && set(_) } && last.foldLeft(true) { _ && set(_) }
      }
      
      prop must pass
    }
    
    "be immutable" in {
      val prop = forAll { (ls: List[String], item: String) =>
        val set = ls.foldLeft(new BloomSet[String](10000, 5)) { _ + _ }
        
        // might fail, but it is doubtful
        (set.accuracy > 0.999 && !ls.contains(item)) ==> {
          val newSet = set + item
          
          ls.foldLeft(true) { _ && set(_) } &&
            !set.contains(item) &&
            ls.foldLeft(true) { _ && newSet(_) } &&
            newSet.contains(item)
        }
      }
      
      prop must pass(set(minTestsOk -> 100, maxDiscarded -> 5000, minSize -> 0, maxSize -> 100))
    }
    
    "construct using companion" in {
      val set = BloomSet("daniel", "chris", "joseph", "renee")
      
      set contains "daniel" mustEqual true
      set contains "chris" mustEqual true
      set contains "joseph" mustEqual true
      set contains "renee" mustEqual true
    }
    
    "implement equivalency" in {
      val prop = forAll { nums: List[Int] =>
        val set1 = nums.foldLeft(BloomSet[Int]()) { _ + _}
        val set2 = nums.foldLeft(BloomSet[Int]()) { _ + _}
        
        set1 == set2
      }
      
      prop must pass
    }
    
    "implement hashing" in {
      val prop = forAll { nums: List[Int] =>
        val set1 = nums.foldLeft(BloomSet[Int]()) { _ + _}
        val set2 = nums.foldLeft(BloomSet[Int]()) { _ + _}
        
        set1.hashCode == set2.hashCode
      }
      
      prop must pass
    }
    
    "persist properly" in {
      skip("Disabling for now")
      
      val prop = forAll { (width: Int, k: Int, ls: List[Int]) => (width > 0 && k > 0) ==> {
          val set = ls.foldLeft(new BloomSet[Int](width, k)) { _ + _ }
          val os = new ByteArrayOutputStream
          
          set.store(os)
          val is = new ByteArrayInputStream(os.toByteArray)
          
          val newSet = BloomSet.load[Int](is)
          
          ls.foldLeft(true) { _ && newSet(_) } &&
              newSet.width == set.width &&
              newSet.k == set.k &&
              newSet.size == set.size
        }
      }
      
      prop must pass
    }
    
    "calculate accuracy" in {
      BloomSet[Int]().accuracy mustEqual 1d
      
      val set = (0 until 1000).foldLeft(BloomSet[Int]()) { _ + _ }
      set.accuracy must beCloseTo(0d, 0.0000001d)
    }
  }
}
