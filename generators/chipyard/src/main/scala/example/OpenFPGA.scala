package chipyard.example

import chisel3._
import chisel3.util._
import chisel3.util.HasBlackBoxPath
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.subsystem.{BaseSubsystem, PBUS}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.{Config, Field, Parameters}

case class OpenFPGADemoParams(
  address: BigInt = OpenFPGAGenerated.BaseAddress,
  size: BigInt = OpenFPGAGenerated.Size,
  bitstreamLength: Int = OpenFPGAGenerated.BitstreamLength,
  cfgWordWidth: Int = OpenFPGAGenerated.CfgWordWidth,
  cfgAddrWidth: Int = OpenFPGAGenerated.CfgAddrWidth,
  cfgDataWidth: Int = OpenFPGAGenerated.CfgDataWidth,
  inputWidth: Int = OpenFPGAGenerated.InputWidth,
  outputWidth: Int = OpenFPGAGenerated.OutputWidth
) {
  require(cfgWordWidth == cfgAddrWidth + cfgDataWidth)
  require(cfgAddrWidth > 0)
  require(cfgDataWidth > 0)
  require(cfgWordWidth > 0 && cfgWordWidth <= 32)
  require(inputWidth > 0 && inputWidth <= 32)
  require(outputWidth > 0 && outputWidth <= 32)
  require(bitstreamLength > 0 && bitstreamLength <= 0xffff)
}

case object OpenFPGADemoKey extends Field[Option[OpenFPGADemoParams]](None)

class OpenFPGAWrapperIO(params: OpenFPGADemoParams) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val cfg_we = Input(Bool())
  val cfg_address = Input(UInt(params.cfgAddrWidth.W))
  val cfg_data = Input(UInt(params.cfgDataWidth.W))
  val user_input = Input(UInt(params.inputWidth.W))
  val user_output = Output(UInt(params.outputWidth.W))
}

class OpenFPGABlackBox(params: OpenFPGADemoParams) extends BlackBox with HasBlackBoxPath {
  val io = IO(new OpenFPGAWrapperIO(params))
  override def desiredName: String = OpenFPGAGenerated.WrapperModule
  addPath(OpenFPGAGenerated.WrapperPath)
}

class OpenFPGATL(params: OpenFPGADemoParams, beatBytes: Int)(implicit p: Parameters)
    extends ClockSinkDomain(ClockSinkParameters())(p) {
  val device = new SimpleDevice(OpenFPGAGenerated.PeripheralName, Seq(OpenFPGAGenerated.Compatible))
  val node = TLRegisterNode(
    Seq(AddressSet(params.address, params.size - 1)),
    device,
    "reg/control",
    beatBytes = beatBytes)

  override lazy val module = new OpenFPGAImpl
  class OpenFPGAImpl extends Impl {
    withClockAndReset(clock, reset) {
      val wrapper = Module(new OpenFPGABlackBox(params))

      val cfgWrite = Wire(Decoupled(UInt(params.cfgWordWidth.W)))
      cfgWrite.ready := false.B

      val controlWrite = Wire(Decoupled(UInt(32.W)))
      controlWrite.ready := true.B

      val userInput = RegInit(0.U(params.inputWidth.W))
      val cfgWord = RegInit(0.U(params.cfgWordWidth.W))
      val cfgCount = RegInit(0.U(16.W))
      val programmed = RegInit(false.B)
      val cfgArm = RegInit(false.B)
      val cfgWe = RegInit(false.B)

      cfgWe := false.B
      when (cfgArm) {
        cfgWe := true.B
        cfgArm := false.B
      }

      cfgWrite.ready := !cfgArm && !cfgWe
      when (cfgWrite.fire) {
        cfgWord := cfgWrite.bits
        cfgArm := true.B
        when (cfgCount =/= 0xffff.U) {
          cfgCount := cfgCount + 1.U
        }
        when ((cfgCount + 1.U) >= params.bitstreamLength.U) {
          programmed := true.B
        }
      }

      when (controlWrite.fire && controlWrite.bits(0)) {
        cfgWord := 0.U
        cfgCount := 0.U
        programmed := false.B
        cfgArm := false.B
        cfgWe := false.B
      }

      wrapper.io.clock := clock
      wrapper.io.reset := reset.asBool
      wrapper.io.cfg_we := cfgWe
      wrapper.io.cfg_address := cfgWord(params.cfgWordWidth - 1, params.cfgDataWidth)
      wrapper.io.cfg_data := cfgWord(params.cfgDataWidth - 1, 0)
      wrapper.io.user_input := userInput

      val cfgActive = cfgArm || cfgWe
      val statusPaddingWidth = 32 - 16 - 2
      val status = Cat(cfgCount, 0.U(statusPaddingWidth.W), cfgActive, programmed)

      node.regmap(
        0x00 -> Seq(RegField.w(32, controlWrite)),
        0x08 -> Seq(RegField.r(32, status)),
        0x10 -> Seq(RegField.w(params.cfgWordWidth, cfgWrite)),
        0x20 -> Seq(RegField(params.inputWidth, userInput)),
        0x28 -> Seq(RegField.r(params.outputWidth, wrapper.io.user_output))
      )
    }
  }
}

trait CanHavePeripheryOpenFPGADemo { this: BaseSubsystem =>
  private val pbus = locateTLBusWrapper(PBUS)

  p(OpenFPGADemoKey).foreach { params =>
    val openfpga = LazyModule(new OpenFPGATL(params, pbus.beatBytes)(p))
    openfpga.clockNode := pbus.fixedClockNode
    pbus.coupleTo(OpenFPGAGenerated.PeripheralName) {
      TLInwardClockCrossingHelper(s"${OpenFPGAGenerated.PeripheralName}_crossing", openfpga, openfpga.node)(
        SynchronousCrossing()) :=
        TLFragmenter(pbus.beatBytes, pbus.blockBytes) := _
    }
  }
}

class WithOpenFPGADemo(address: BigInt = OpenFPGAGenerated.BaseAddress) extends Config((site, here, up) => {
  case OpenFPGADemoKey => Some(OpenFPGADemoParams(address = address))
})
