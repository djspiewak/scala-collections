import org.specs._
import org.scalacheck._

import com.codecommit.collection.HashMap

object HashMapSpecs extends Specification with Scalacheck {
  import Prop._
  
  "it" should {
    "store ints" in {
      val prop = property { src: List[Int] =>
        val map = src.foldLeft(new HashMap[Int, Int]) { (m, v) => m(v) = -v }
        src forall { v => map(v) == -v }
      }
      
      prop must pass
    }
    
    "store strings" in {
      val prop = property { src: List[String] =>
        val map = src.foldLeft(new HashMap[String, Int]) { (m, v) => m(v) = v.length }
        src forall { v => map(v) == v.length }
      }
      
      prop must pass
    }
    
    
  }
  
  /* implicit def arbHashMap[K, V](implicit ak: Arbitrary[List[K]], implicit av: Arbitrary[(K)=>V]): Arbitrary[HashMap[K, V]] = {
    Arbitrary(for {
      keys <- ak.arbitrary
      f <- av.arbitrary
    } yield keys.foldLeft(new HashMap[K, V]) { (m, k) => m(k) = f(k) })
  } */
}
