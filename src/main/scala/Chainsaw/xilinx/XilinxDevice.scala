package Chainsaw.xilinx

import Chainsaw._
import Chainsaw.edaFlow._
import spinal.core.HertzNumber

import java.io.File
import scala.language.postfixOps

/** @param family
  *   device family
  * @param part
  *   full par name
  * @param fMax
  *   recommended fmax
  * @param xdcFile
  *   xdc file for specific board
  */
case class XilinxDevice(
    family: XilinxDeviceFamily,
    part: String,
    fMax: HertzNumber,
    xdcFile: Option[File],
    budget: VivadoUtil = VivadoUtil()
) {

  def bramCapacity = budget.bram36 * 36 * 1024 / pow2(20).toDouble

  def uramCapacity = budget.uram288 * 288 * 1024 / pow2(20).toDouble

  def onChipStorage = bramCapacity + uramCapacity

}
