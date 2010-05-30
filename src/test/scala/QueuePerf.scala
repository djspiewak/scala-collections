import com.codecommit.collection.BankersQueue
import scala.collection.immutable.Queue

object QueuePerf {
  import PerfLib._
  
  def main(args: Array[String]) {
    println()
    
    // ==========================================================================
    {
      title("Enqueue 100,000 values")
      
      val data = new Array[Int](100000)
      for (i <- 0 until data.length) {
        data(i) = Math.round(Math.random).toInt
      }
      
      val bankersQueueOp = "BankersQueue" -> time {
        var q = BankersQueue[Int]()
        var i = 0
        
        while (i < data.length) {
          q += data(i)
          i += 1
        }
      }
      
      val queueOp = "Queue" -> time {
        var q = Queue[Int]()
        var i = 0
        
        while (i < data.length) {
          q = q enqueue data(i)
          i += 1
        }
      }
      
      bankersQueueOp compare queueOp
      div('=')
    }
    
    // ==========================================================================
    {
      title("Dequeue 100,000 values")
      
      val data = new Array[Int](100000)
      for (i <- 0 until data.length) {
        data(i) = Math.round(Math.random).toInt
      }
      
      val bq = (BankersQueue[Int]() /: data) { _ + _ }
      val rq = (Queue[Int]() /: data) { _ enqueue _ }
      
      val bankersQueueOp = "BankersQueue" -> time {
        var q = bq
        var i = 0
        
        while (i < data.length) {
          q = q.dequeue._2
          i += 1
        }
      }
      
      val queueOp = "Queue" -> time {
        var q = rq
        var i = 0
        
        while (i < data.length) {
          q = q.dequeue._2
          i += 1
        }
      }
      
      bankersQueueOp compare queueOp
      div('=')
    }
    
    // ==========================================================================
    {
      title("Enqueue AND Dequeue 100,000 values")
      
      val data = new Array[Int](100000)
      for (i <- 0 until data.length) {
        data(i) = Math.round(Math.random).toInt
      }
      
      val bankersQueueOp = "BankersQueue" -> time {
        var q = BankersQueue[Int]()
        var i = 0
        
        while (i < data.length) {
          q += data(i)
          i += 1
        }
        
        i = 0
        
        while (i < data.length) {
          q = q.dequeue._2
          i += 1
        }
      }
      
      val queueOp = "Queue" -> time {
        var q = Queue[Int]()
        var i = 0
        
        while (i < data.length) {
          q = q enqueue data(i)
          i += 1
        }
        
        while (i < data.length) {
          q = q.dequeue._2
          i += 1
        }
      }
      
      bankersQueueOp compare queueOp
      div('=')
    }
  }
}
