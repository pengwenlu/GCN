package Chainsaw.GCN

import Chainsaw.GCN.{PE, meshCols, meshRows, partialSumWidth, quantifyWidth}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._

case class SystolicArray (meshRows: Int = meshRows, meshCols: Int = meshCols) extends  Component {
  val io = new Bundle{
    val inAct = Vec(slave Flow SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp), meshRows)
    val inConfig = Vec(slave Flow Bits(configBitNumber bits), meshRows) //inAct
    val indown = Vec(slave Flow Bits(downBitNumber bits), meshRows)

    val inWt = slave Flow Vec(Vec(SFix(peak = 0 exp, resolution = -(quantifyWidth-1) exp), 3), meshCols) //in same valid
    val inPs = slave Flow Vec(SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp), meshCols)

    val weightSwitch = in Bool()  // ping-pong control(rising edge valid)

    val outPs = master Flow Vec(SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp), meshCols)  //in same valid
    val MR = Vec(Vec(master Flow SFix(peak = 0 exp, resolution = -(partialSumWidth-1) exp),meshRows), meshCols)
    val oConfig = Vec(Vec(master Flow Bits(configBitNumber bits), meshRows), meshCols) //MR

  }

  // tabulate is to generate row rows, col cols array
  val peWsArray = Array.tabulate(meshRows, meshCols)((row, col) => PE(col==meshCols-1, row==meshRows-1))

  // inAct
  for(row <- 0 until meshRows) {
    for(col <- 0 until meshCols) {
      if(col==0){
        //the first column
        peWsArray(row)(col).io.inAct.payload := io.inAct(row).payload.d(row)//da pai
        peWsArray(row)(col).io.inAct.valid := io.inAct(row).valid.d(row)
        peWsArray(row)(col).io.inConfig.payload := io.inConfig(row).payload.d(row)
        peWsArray(row)(col).io.inConfig.valid := io.inConfig(row).valid.d(row)
        peWsArray(row)(col).io.indown.payload := io.indown(row).payload.d(row)
        peWsArray(row)(col).io.indown.valid := io.indown(row).valid.d(row)
      }
      else {
        peWsArray(row)(col).io.inAct := peWsArray(row)(col-1).io.outAct //inside da pai
        peWsArray(row)(col).io.inConfig := peWsArray(row)(col-1).io.outConfig.d()//outside da pai
        peWsArray(row)(col).io.indown := peWsArray(row)(col-1).io.outdown.d()
      }
    }
  }

  // inWt
  for(row <- 0 until meshRows) {
    for(col <- 0 until meshCols) {
      //the first row
      if(row==0) peWsArray(row)(col).io.inWt.payload := io.inWt.payload(col)
      //others rows
      else peWsArray(row)(col).io.inWt.payload := peWsArray(row-1)(col).io.outWt
      peWsArray(row)(col).io.inWt.valid := io.inWt.valid  // valid signal broadcast
    }
  }

  // inPs, two dimension, in peWsArray.
  for(row <- 0 until meshRows) {
    for (col <- 0 until meshCols) {
      if (row != 0)
        peWsArray(row)(col).io.inPs := peWsArray(row - 1)(col).io.outPs
    }
  }

  //not every row, so dont need for, just the first row is needed to add inPS0
  //(PE0,0) find inPs0, firstRow_2 is 0 correspond to inPs.payload(0)[inPs0]
  peWsArray.head.zipWithIndex.foreach(firstRow => {
    firstRow._1.io.inPs.payload := io.inPs.payload(firstRow._2).d(firstRow._2)
    firstRow._1.io.inPs.valid := io.inPs.valid.d(firstRow._2)
  })

  // outPs
  //lastRow: (PE, Int), so outPs0 correspond to (PE0,0) output
  peWsArray.last.zipWithIndex.foreach(lastRow => {
    io.outPs.payload(lastRow._2) := lastRow._1.io.outPs.payload.d(meshCols-lastRow._2-1)
    //io.oConfig.equals(io.inConfig)
  })
  io.outPs.valid := peWsArray.last.last.io.outPs.valid


  // weightSwitch
  for(i <- 0 until meshRows) {
    for(j <- 0 until meshCols) {
      peWsArray(i)(j).io.weightSwitch := io.weightSwitch
    }
  }

  //MR
  //two-dimension, outside peWsArray
  for (col <- 0 until meshCols) {
    for (row <- 0 until meshRows) {
      io.MR(row)(col).payload := peWsArray(row)(col).io.currentMR.payload
      io.MR(row)(col).valid := peWsArray(row)(col).io.currentMR.valid
      io.oConfig(row)(col).payload := peWsArray(row)(col).io.outConfig.payload.d() //MR.payload da pai, so oConfig also da pai
      io.oConfig(row)(col).valid := peWsArray(row)(col).io.currentMR.valid // write in oConfig.valid is equal to currentMR.valid
    }
  }

}

object SystolicArray {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(SystolicArray()).printPruned()}}
