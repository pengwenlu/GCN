package Chainsaw.GCN

import Chainsaw.GCN.{partialSumWidth, PE, quantifyWidth}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

case class PE (rightBoundary: Boolean, bottomBoundary: Boolean) extends Component {
  val io = new Bundle{
    val inAct = slave Flow SFix(peak= 0 exp, resolution = -(quantifyWidth-1) exp) //sign, flow has valid
    val inConfig = slave Flow Bits(configBitNumber bits)
    val indown = slave Flow Bits(downBitNumber bits)
    val inWt = slave Flow Vec(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp), 3)//ping-pong
    val inPs = slave Flow SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp)


    val weightSwitch = in Bool() //ping-pong control(rising edge valid)


    val outAct = (!rightBoundary).generate(master Flow SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp))
    // ping-pong, outWt is all controlled by a global valid signal, only inWt is flow with valid, outWt is out
    val outWt = (!bottomBoundary).generate(out Vec(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp), 3))
    val outPs = master Flow SFix(peak= 0 exp, resolution = -(partialSumWidth-1) exp)
    val currentMR = master Flow SFix (peak = 0 exp, resolution = -(partialSumWidth-1) exp)
    val outConfig = master Flow Bits(configBitNumber bits)
    val outdown = master Flow Bits(downBitNumber bits)
  }

  //weight logic
  val weightReg0 = Reg(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp))
  val weightReg1 = Reg(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp))
  val weightReg2 = Reg(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp))
  when(io.weightSwitch.rise()){
    weightReg0 := weightReg1
    weightReg1 := weightReg2
    weightReg2 := weightReg2
  }
  when(io.inWt.valid){
    weightReg0 := io.inWt.payload(0)
    weightReg1 := io.inWt.payload(1)
    weightReg2 := io.inWt.payload(2)
  }

  //when 0 ,inAct has value, outAct is 0
  val actReg = (!rightBoundary).generate(RegNextWhen(io.inAct.payload, io.inAct.valid))
  val currentMRReg = Reg(SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp))init(0)
  val psReg = Reg(SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp))init(0)

  //outAct logic
  (!rightBoundary).generate{
    io.outAct.payload := actReg
    io.outAct.valid := io.inAct.valid.d() init(False)
  }

  //MC logic
  /*
  1.0:              inAct.valid=0, inAct.payload = 0; down.valid=0, down.payload =0; inConfig.valid=0, inConfig.payload=0
  2.dont need add:  inAct.valid=1, inAct.payload = 1; down.valid=0, down.payload =0; inConfig.valid=1, inConfig.payload=1
  3.need add:       inAct.valid=1, inAct.payload = 1; down.valid=1, down.payload =1; inConfig.valid=1, inConfig.payload=1
   */
  when(io.inAct.valid) {
    when(io.indown.valid) {
      psReg := (io.inAct.payload * weightReg0 + io.inPs.payload).truncated
    } otherwise {
      psReg := io.inPs.payload
      currentMRReg := (io.inAct.payload * weightReg0).truncated
    }
  }otherwise{
    psReg := io.inPs.payload
  }

  //weight logic
  (!bottomBoundary).generate{
    io.outWt(0) := weightReg0
    io.outWt(1) := weightReg1
    io.outWt(2) := weightReg2
  }

  io.outPs.payload := psReg
  io.outPs.valid := io.inAct.valid.d()
  io.outConfig := io.inConfig
  io.currentMR.payload := currentMRReg
  io.currentMR.valid := (io.indown.valid ^ io.inAct.valid).d() init(False)
  io.outdown := io.indown
}

object PE {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(PE(false, false)).printPruned()
  }
}