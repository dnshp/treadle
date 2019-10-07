// See LICENSE for license details.

package treadle.executable

import treadle._
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}
import treadle.{
  DataStorePlugInAnnotation,
  TreadleTester
}

import scala.collection.mutable

class ActivityFactorSpec extends FreeSpec with Matchers {

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
          |    d <= add(r, a)
          |""".stripMargin

      val activityFactorCollector = new ActivityFactorCollector
      val annos = Seq(
        DataStorePlugInAnnotation("ActivityFactorCollector", activityFactorCollector.getPlugin)
      )
      val tester = TreadleTester(annos :+ FirrtlSourceAnnotation(input))

      for (a <- 1 to 10) {
        tester.poke("a", a)
        tester.poke("b", a)
        tester.step()
      }
      tester.finish

      activityFactorCollector.signals("r").transitionCount should be (BigInt(17))
      activityFactorCollector.report(tester.engine)
    }
  }
}
