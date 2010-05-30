import com.codecommit.collection.BankersQueue

import org.specs._
import org.scalacheck._

object BankersQueueSpecs extends Specification with ScalaCheck {
  import Prop._
  
  "BankersQueue" should {
    "adhear to fifo" in {
      val prop = forAll { xs: List[Int] =>
        val q = (BankersQueue[Int]() /: xs) { _ + _ }
        xs.foldLeft(q) { (q, i) =>
          val (i2, q2) = q.dequeue
          i2 mustEqual i
          q2
        }
        true
      }
      
      prop must pass
    }
    
    "define apply from head" in {
      val prop = forAll { (i: Int, xs: List[Int]) =>
        !xs.isEmpty ==> {
          val idx = abs(i % xs.length)
          val q = (BankersQueue[Int]() /: xs) { _ + _ }
          q(idx) mustEqual xs(idx)
        }
      }
      
      prop must pass
    }
  }
  
  def abs(i: Int) = Math.abs(i)          // workaround for scalac bug
}
