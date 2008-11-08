import com.codecommit.collection.HashMap

object HashPerf {
  import PerfLib._
  
  def main(args: Array[String]) {
    println()
    
    //==========================================================================
    {
      title("Fill 100000 Random Keys")
      
      val indexes = new Array[Int](100000)
      var max = -1
      for (i <- 0 until indexes.length) {
        indexes(i) = Math.round(Math.random * 40000000).toInt
        max = Math.max(max, indexes(i))
      }
      
      val data = "The quick brown fox jumped over the lazy red dog"
      
      val hashMapOp = "HashMap" -> time {
        var map = HashMap[Int, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map + (i -> data)
          i += 1
        }
      }
      
      val mapOp = "Map" -> time {
        var map = Map[Int, String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map + (i -> data)
          i += 1
        }
      }
      
      hashMapOp compare mapOp
      
      val intMapOp = "Map" -> time {
        var map = test.IntMap[String]()
        var i = 0
        
        while (i < indexes.length) {
          map = map(i) = data
          i += 1
        }
      }
      
      hashMapOp compare intMapOp
    }
  }
}
