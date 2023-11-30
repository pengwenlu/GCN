package Chainsaw

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

package object GCN {
  implicit class DataUtil[T <: Data] (data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle) // default beat one phase
  }

  val configBitNumber: Int = 4
  val downBitNumber: Int = 2

  // parameters
  // data width
  val quantifyWidth: Int = 8  // include sign bit
  val partialSumWidth: Int = 2 * quantifyWidth  // include sign bit

  // systolic array height and width
  val meshRows: Int = 4  // test size: 8/16, support size: 8/16/32
  val meshCols: Int = meshRows

}
