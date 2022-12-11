package Chainsaw.dsp

import Chainsaw._
import Chainsaw.memory.DynamicTimeOut
import Chainsaw.xilinx._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

// TODO: parallel version
case class DynamicDelay(delay: Int, dataType: NumericTypeNew, parallel: Int)
  extends ChainsawDynamicInfiniteGenerator {

  override def name = s"DynamicDelay_$delay"

  //  override def model = new ChainsawInfiniteModel {
  //    override def impl(data: Seq[BigDecimal]) =
  //      if (!dynamic) data
  //      else data.grouped(parallel + 1).flatMap(_.init).toSeq
  //
  //    override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
  //      correlationMetric(yours, golden, 0.99)
  //
  //    override def testCases =
  //      if (!dynamic) Seq.fill(1000)(dataType.random)
  //      else Seq.fill(1000)(Seq.fill(parallel)(dataType.random) :+ BigDecimal(delay / 3)).flatten
  //
  //    override def latency = delay / parallel
  //
  //    override def offset = delay % parallel
  //  }

  /** --------
   * model
   * -------- */
  override def impl(testCase: TestCase) = testCase.data

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) = yours.equals(golden)

  override def testCases = {
    // test for 20 segments
    val controls = Seq.fill(20)((Random.nextInt(delay) + 4) min delay).map(ctrl => Seq(BigDecimal(ctrl)))
    controls.map(ctrl => TestCase(Seq.fill(100)(randomDataVector).flatten, ctrl))
  }

  override def latency(control: Seq[BigDecimal]) = control.head.toInt

  override def controlTypes = Seq(NumericTypeNew.U(log2Up(delay + 1)))

  override def inputTypes = Seq.fill(parallel)(dataType)

  override def outputTypes = Seq.fill(parallel)(dataType)

  override def vivadoUtilEstimation = VivadoUtilEstimation()

  override def fmaxEstimation = 600 MHz

  override def implH = new ChainsawDynamicInfiniteModule(this) {
    // TODO: for parallel > 1, using multiple following structures with different delays
    require(parallel == 1)
    // TODO: assertion for delay >= 4
    val control = controlIn.head.asUInt()
    val currentLatency = RegNextWhen(control, validIn.rise())
    val counterWidth = log2Up(delay + 1)
    val paddedLength = Pow2(counterWidth)
    val indexCounter = CounterFreeRun(paddedLength)

    val flowWrite = cloneOf(flowOut)
    flowWrite.fragment := dataIn
    flowWrite.valid := validIn
    flowWrite.last := lastIn

    val ram = Mem(HardType(flowOut), paddedLength)
    ram.write(indexCounter.value, flowWrite)
    val readAddr = (indexCounter.value - (currentLatency - 3)).d()
    val readData = ram.readSync(readAddr).d()

    val timeOut = DynamicTimeOut(control)
    when(validIn.rise())(timeOut.clear())

    dataOut := readData.fragment
    validOut := Mux(timeOut.state, readData.valid, False)
    lastOut := Mux(timeOut, readData.last, False)
  }

  override def implNaiveH = None

  override def resetCycle = delay
}