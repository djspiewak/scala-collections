import org.specs._
import org.scalacheck._

import com.codecommit.collection.HashTrie

object HashTrieSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "it" should {
    "store ints" in {
      val prop = forAll { src: List[Int] =>
        val map = src.foldLeft(new HashTrie[Int, Int]) { (m, v) => m(v) = -v }
        src forall { v => map(v) == -v }
      }
      
      prop must pass(set(maxSize -> 5000, minTestsOk -> 2000))
    }
    
    "store strings" in {
      val prop = forAll { src: List[String] =>
        val map = src.foldLeft(new HashTrie[String, Int]) { (m, v) => m(v) = v.length }
        src forall { v => map(v) == v.length }
      }
      
      prop must pass
    }
    
    "preserve values across changes" in {
      val prop = forAll { (map: HashTrie[String, String], ls: List[String], f: (String)=>String) =>
        val filtered = ls filter { !map.contains(_) }
        
        filtered.length > 0 ==> {
          val newMap = filtered.foldLeft(map) { (m, k) => m(k) = f(k) }
          
          (map forall { case (k, v) => newMap(k) == v }) && (filtered forall { v => newMap(v) == f(v) })
        }
      }
      
      prop must pass
    }
    
    "calculate size" in {
      val prop = forAll { (ls: Set[Int], f: (Int)=>Int) =>
        val map = ls.foldLeft(new HashTrie[Int, Int]) { (m, v) => m(v) = f(v) }
        map.size == ls.size
      }
      
      prop must pass
    }
    
    "remove ints" in {
      val prop = forAll { map: HashTrie[Int, String] =>
        map.size > 0 ==> {
          val (rm, _) = map.elements.next     // insufficient
          val newMap = map - rm
          
          !newMap.contains(rm) && 
            (newMap forall { case (k, v) => map(k) == v }) && 
            newMap.size == map.size - 1
        }
      }
      
      prop must pass
    }
    
    "remove strings" in {
      val prop = forAll { map: HashTrie[String, String] =>
        map.size > 0 ==> {
          val (rm, _) = map.elements.next
          val newMap = map - rm
          
          !newMap.contains(rm) && 
            (newMap forall { case (k, v) => map(k) == v }) && 
            newMap.size == map.size - 1
        }
      }
      
      prop must pass
    }
    
    "define empty" in {
      val prop = forAll { map: HashTrie[String, String] =>
        map.empty.size == 0
      }
      
      prop must pass
    }
  }
  
  implicit def arbHashTrie[K](implicit ak: Arbitrary[List[K]]): Arbitrary[HashTrie[K, String]] = {
    Arbitrary(for {
      keys <- ak.arbitrary
    } yield keys.foldLeft(new HashTrie[K, String]) { (m, k) => m(k) = k.toString })
  }
  
  implicit def arbSet[A](implicit arb: Arbitrary[List[A]]): Arbitrary[Set[A]] = {
    Arbitrary(for {
      ls <- arb.arbitrary
    } yield ls.foldLeft(Set[A]()) { _ + _ })
  }
}
