import org.specs._

import com.codecommit.collection.HashMap

object HashMapSpecs extends Specification {
  "it" should {
    "not die" in {
      (0 until 33).foldLeft(new HashMap[Int, Int]) { (m, n) => m(n) = -n }
    }
  }
}
