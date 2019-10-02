// See LICENSE for license details.

package treadle.executable

import scala.collection.mutable

case class ActivityFactor(transitionCount: BigInt, clockCount: BigInt) {
  def update(value: BigInt, previousValue: BigInt, bits: BigInt): ActivityFactor = {
    if (value != previousValue) {
      ActivityFactor(transitionCount + bitDiff(value.toInt, previousValue.toInt, bits.toInt), clockCount + 1)
    } else {
      ActivityFactor(transitionCount, clockCount + 1)
    }
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

  case class PlugIn(executionEngine: ExecutionEngine) extends DataStorePlugin {
    override def dataStore: DataStore = executionEngine.dataStore

    override def run(symbol: Symbol, offset: Int, previousValue: Big): Unit = {
      signals(symbol.name) = signals.get(symbol.name) match {
        case Some(activity) => activity.update(dataStore(symbol), previousValue, symbol.bitWidth)
        case None           => ActivityFactor(0, 1) // Initialize on the first cycle
      }
    }
  }
}

