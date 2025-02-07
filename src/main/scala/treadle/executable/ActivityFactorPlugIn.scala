// See LICENSE for license details.

package treadle.executable

import scala.collection.mutable
import reporters._

case class ActivityFactor(transitionCount: BigInt, clockCount: BigInt, lastVal: BigInt) {
  def update(value: BigInt, previousValue: BigInt, bits: BigInt): ActivityFactor = {
    ActivityFactor(transitionCount + bitDiff(value.toInt, lastVal.toInt, bits.toInt), clockCount + 1, value)
  }

  def bitDiff(a: Int, b: Int, bits: Int): Int = {
    var count = 0
    for (bit <- 0 to bits) {
      if (((a ^ b) & (1 << bit)) != 0) count += 1
    }
    return count
  }

  def activityFactor: Float = {
    return transitionCount.toFloat / clockCount.toFloat
  }
}

class ActivityFactorCollector {
  val signals = new mutable.HashMap[String, ActivityFactor]
  def activityFactor(signal: String): Float = {
    return signals(signal).activityFactor
  }

  def getPlugin(executionEngine: ExecutionEngine): DataStorePlugin = {
    PlugIn(executionEngine)
  }

  def report(executionEngine: ExecutionEngine) {
    val reporter = new ReportArea
    val opMap = reporter.moduleOpInputsMap(executionEngine.ast)
    for ((name, collector) <- signals) {
      println(s"Signal ${name} has power ${calcPower(name, collector, opMap, "PassThrough")}")
    }
  }

  def calcPower(name : String, collector : ActivityFactor, opMap : mutable.Map[String,mutable.ListBuffer[PowerAnnotation]], moduleName : String): Float = {
    val powerAnno = opMap(moduleName).find(_.name == name)
    if (powerAnno == None) {
      println(s"Signal ${name} not found in ledger!")
      0
    }
    else ComputePower(powerAnno.get, collector.activityFactor)
  }

  case class PlugIn(executionEngine: ExecutionEngine) extends DataStorePlugin {
    override def dataStore: DataStore = executionEngine.dataStore

    override def run(symbol: Symbol, offset: Int, previousValue: Big): Unit = {
      if ((executionEngine.getValue("clock/prev") == 0) && (executionEngine.getValue("clock") == 1)) {
        signals(symbol.name) = signals.get(symbol.name) match {
          case Some(activity) => activity.update(dataStore(symbol), previousValue, symbol.bitWidth)
          case None           => ActivityFactor(0, 1, dataStore(symbol)) // Initialize on the first cycle
        }
      }
    }
  }
}

