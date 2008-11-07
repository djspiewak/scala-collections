import org.specs._
import org.scalacheck._

import com.codecommit.collection.HashMap

object HashMapSpecs extends Specification with Scalacheck {
  import Prop._
  
  "it" should {
    "store objects" in {
      val prop = property { src: Map[Any, Any] =>
        val map = src.foldLeft(new HashMap[Any, Any]) { _ + _ }
        src forall { case (k, v) => map(k) == v }
      }
      
      prop must pass
    }
  }
}
