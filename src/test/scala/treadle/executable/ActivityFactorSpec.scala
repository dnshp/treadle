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

class ActivityFactorSpec extends FreeSpec with Matchers {

  def flatten(input : String, topName : String): String = {
      val inputCirc = firrtl.Parser.parse(input)
      val name = ModuleName(topName, CircuitName(topName)) // If this fails, bad input
      val flatteningAnnotation = Seq[Annotation](FlattenAnnotation(name))
      val finalState = (new LowFirrtlCompiler).compileAndEmit(CircuitState(inputCirc, ChirrtlForm, flatteningAnnotation), Seq[Transform](new Flatten))
      val flattenedCirc = RemoveEmpty.run(Parser.parse(finalState.getEmittedCircuit.value)).serialize
      flattenedCirc
  }

  info( "this")

  "Tracking activity factor of signals" - {
    "intended for use in dynamic power estimation" in {
      val input =
        """
          |circuit PassThrough :
          |  module PassThrough :
          |    input clock : Clock
          |    input a : SInt<8>
          |    input b : SInt<8>
          |    output c: SInt<9>
          |    output d: SInt<10>
          |
          |    reg r : SInt<9>, clock
          |    r <= add(a, b)
          |    c <= add(a, b)
          |    inst adder of Add
          |    adder.r <= r
          |    adder.a <= a
          |    d <= adder.d
          |
          |  module Add :
          |    input r : SInt<9>
          |    input a : SInt<8>
          |    output d : SInt<10>
          |
          |    d <= add(r, a)
          |""".stripMargin

      val flattenedCirc = flatten(input, "PassThrough")
      println(flattenedCirc)
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
