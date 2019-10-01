// See LICENSE for license details.

package treadle.executable

import firrtl.annotations.NoTargetAnnotation
import firrtl.options.Unserializable
import treadle.vcd.VCD

import scala.collection.mutable

abstract class DataStorePlugin {
  def executionEngine: ExecutionEngine
  def dataStore: DataStore
  var isEnabled: Boolean = false

  def setEnabled(enabled: Boolean): Unit = {
    isEnabled = enabled
    (isEnabled, dataStore.activePlugins.contains(this)) match {
      case (true, false) =>
        dataStore.activePlugins += this
      case (false, true) =>
        dataStore.activePlugins.remove(dataStore.activePlugins.indexOf(this))
      case _ =>
        /* do nothing */
    }
    if(dataStore.activePlugins.nonEmpty && dataStore.leanMode) {
      dataStore.leanMode = false
      executionEngine.scheduler.getAllAssigners.foreach { assigner => assigner.setLeanMode(false)}
    }
    else if(dataStore.activePlugins.isEmpty && ! dataStore.leanMode) {
      dataStore.leanMode = true
      executionEngine.scheduler.getAllAssigners.foreach { assigner => assigner.setLeanMode(true)}
    }
  }

  def run(symbol: Symbol, offset: Int = -1, previousValue: Big): Unit
}

class ReportAssignments(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if(offset == -1) {
      val valueMessage = if(symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      }
      else {
        val showValue = symbol.normalize(dataStore(symbol))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name} <= $valueMessage")
    }
    else {
      val valueMessage = if(symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      }
      else {
        val showValue = symbol.normalize(dataStore(symbol, offset))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name}($offset) <= $valueMessage")
    }
  }
}

class RenderComputations(
  val executionEngine: ExecutionEngine,
  symbolNamesToWatch: Seq[String]
) extends DataStorePlugin {

  val dataStore: DataStore = executionEngine.dataStore
  val symbolsToWatch: Set[Symbol] = symbolNamesToWatch.flatMap { name =>
    executionEngine.symbolTable.get(name)
  }.toSet

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if(symbolsToWatch.contains(symbol)) {
      if(symbol.forcedValue.isDefined) {
        print(s"FORCED(${symbol.forcedValue.get} would have been: ")
      }
      println(executionEngine.renderComputation(symbol.name))
    }
  }
}

class VcdHook(val executionEngine: ExecutionEngine, val vcd: VCD) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  override def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if(offset == -1) {
      val value = dataStore(symbol)
      vcd.wireChanged(symbol.name, value, width = symbol.bitWidth)
    }
  }
}

case class ActivityFactor(transitionCount: BigInt) {
  def update(value: BigInt, previousValue: BigInt): ActivityFactor = {
    if (value != previousValue) {
      ActivityFactor(transitionCount + 1)
    } else {
      ActivityFactor(transitionCount)
    }
  }
}

class ActivityFactorCollector {
  val activity = new mutable.HashMap[String, ActivityFactor]
  def getPlugin(executionEngine: ExecutionEngine): DataStorePlugin = {
    PlugIn(executionEngine)
  }

  case class PlugIn(executionEngine: ExecutionEngine) extends DataStorePlugin {
    override def dataStore: DataStore = executionEngine.dataStore

    override def run(symbol: Symbol, offset: Int, previousValue: Big): Unit = {
      activity(symbol.name) = activity.get(symbol.name) match {
        case Some(activity) => activity.update(dataStore(symbol), previousValue)
        case None           => ActivityFactor(1)
      }
    }
  }
}
