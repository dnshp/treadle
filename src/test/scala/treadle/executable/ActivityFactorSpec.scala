// See LICENSE for license details.

package treadle.executable

import treadle._
import firrtl.stage.FirrtlSourceAnnotation
import firrtl.transforms.{FlattenAnnotation, Flatten}
import org.scalatest.{FreeSpec, Matchers}
import treadle.{
  DataStorePlugInAnnotation,
  TreadleTester
}

import scala.collection.mutable
import reporters.{ReportArea, AreaAnnotation}
import firrtl._
import firrtl.Parser.UseInfo
import firrtl.passes.RemoveEmpty
import firrtl.annotations.{ModuleName, CircuitName, ComponentName, Annotation}
import scala.io.Source

class ActivityFactorSpec extends FreeSpec with Matchers {

  def flatten(input : String): String = {
      val inputCirc = firrtl.Parser.parse(input)
      val name = ModuleName(inputCirc.main, CircuitName(inputCirc.main)) // If this fails, bad input
      val flatteningAnnotation = Seq[Annotation](FlattenAnnotation(name))
      val finalState = (new LowFirrtlCompiler).compileAndEmit(CircuitState(inputCirc, ChirrtlForm, flatteningAnnotation), Seq[Transform](new Flatten))
      val flattenedCirc = RemoveEmpty.run(Parser.parse(finalState.getEmittedCircuit.value)).serialize
      flattenedCirc
  }

  info( "this")

  "Tracking activity factor of signals" - {
    "intended for use in dynamic power estimation" in {

      val input = Source.fromFile("samples/PassThrough.fir").mkString
      val flattenedCirc = flatten(input)
      val activityFactorCollector = new ActivityFactorCollector
      val annos = Seq(
        DataStorePlugInAnnotation("ActivityFactorCollector", activityFactorCollector.getPlugin)
      )
      val tester = TreadleTester(annos :+ FirrtlSourceAnnotation(flattenedCirc))

      val expectedTransitionCount = Seq(0, 2, 3, 6, 7, 9, 10, 14, 15, 17)

      for (a <- 1 to 10) {
        tester.poke("a", a)
        tester.poke("b", a)
        tester.step()
        activityFactorCollector.signals("r").transitionCount should be (BigInt(expectedTransitionCount(a - 1)))
      }
      tester.finish

      activityFactorCollector.report(tester.engine)
    }
  }
}
