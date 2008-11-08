object PerfLib {
  @inline
  def time(op: =>Unit) = {
    var data = new Array[Test](12)
    
    var minTime = Math.MAX_LONG
    var minMem = Math.MAX_LONG
    var minTimeI = 0
    var minMemI = 0
    
    var maxTime = Math.MIN_LONG
    var maxMem = Math.MIN_LONG
    var maxTimeI = 1
    var maxMemI = 1
    
    for (i <- 0 until data.length) {
      val mem = Runtime.getRuntime.freeMemory
      val start = System.nanoTime
      op
      data(i) = Test(System.nanoTime - start, mem - Runtime.getRuntime.freeMemory)
      
      if (data(i).nanos < minTime) {
        minTime = data(i).nanos
        minTimeI = i
      }
      if (data(i).bytes < minMem) {
        minMem = data(i).bytes
        minMemI = i
      }
      
      if (data(i).nanos > maxTime) {
        maxTime = data(i).nanos
        maxTimeI = i
      }
      if (data(i).bytes > maxMem) {
        maxMem = data(i).bytes
        maxMemI = i
      }
      
      Runtime.getRuntime.gc()
    }
    
    var sumTime: BigInt = 0
    var sumMem: BigInt = 0
    for (i <- 0 until data.length) {
      if (i != minTimeI && i != maxTimeI) {
        sumTime += data(i).nanos
      }
      
      if (i != minMemI && i != maxMemI) {
        sumMem += data(i).bytes
      }
    }
    
    Test((sumTime / (data.length - 2)).longValue, (sumMem / (data.length - 2)).longValue)
  }
  
  def title(str: String) {
    div('=')
    println("  " + str)
  }
  
  def div(c: Char) = {
    for (i <- 0 until 80) print(c)
    println()
  }
  
  implicit def opToComparison(op1: (String, Test)) = new ComparisonClass(op1)
  
  class ComparisonClass(op1: (String, Test)) {
    private val timeTemplate = "%%%ds Time: %%fms%n"
    private val memoryTemplate = "%%%ds Memory: %%f KB%n"
    
    private val diff = "Time Difference"
    private val memDiff = "Memory Difference"
    private val diffTemplate = "%%%ds: %%%%f%%s%n"
    
    private val percent = "Percentage Difference"
    private val percentTemplate = "%%%ds: %%%%f%%%%%%%%%n"
    
    def compare(op2: (String, Test)) {
      import Math.max
      
      val (op1Name, Test(op1Time, op1Mem)) = op1
      val (op2Name, Test(op2Time, op2Mem)) = op2
      
      val widthTotal = max(op1Name.length + 7, max(op2Name.length + 7, max(diff.length, max(memDiff.length, percent.length)))) + 2
      
      val width:java.lang.Integer = widthTotal
      val timeWidth:java.lang.Integer = widthTotal - 5
      val memWidth:java.lang.Integer = widthTotal - 7
      
      val timeFormat = String.format(timeTemplate, Array(timeWidth))
      val memoryFormat = String.format(memoryTemplate, Array(memWidth))
      val diffFormat = String.format(String.format(diffTemplate, Array(width)), Array(diff, "ms"))
      val memDiffFormat = String.format(String.format(diffTemplate, Array(width)), Array(memDiff, " KB"))
      val percentFormat = String.format(String.format(percentTemplate, Array(width)), Array(percent))
      
      div('-')
      
      printf(timeFormat, op1Name, (op1Time:Double) / 1000000)
      printf(timeFormat, op2Name, (op2Time:Double) / 1000000)
      
      printf(memoryFormat, op1Name, (op1Mem:Double) / 1024)
      printf(memoryFormat, op2Name, (op2Mem:Double) / 1024)
      
      println()
      printf(diffFormat, ((op1Time - op2Time):Double) / 1000000)
      printf(percentFormat, (((op1Time:Double) / (op2Time:Double)) - 1) * 100)
      
      println()
      printf(memDiffFormat, ((op1Mem - op2Mem):Double) / 1024)
    }
  }
  
  case class Test(nanos: Long, bytes: Long)
}
