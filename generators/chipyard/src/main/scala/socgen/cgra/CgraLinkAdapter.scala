package chipyard.socgen.cgra

import chisel3._
import chisel3.util._
import chipyard.example.{CGRACmdGenerated, CGRAParams}
import chipyard.socgen.link._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.{Config, Field}

object CgraLinkStatus {
  val BadConfig = 1
  val BadPacket = 2
  val DmaMismatch = 3
}

case class CgraLinkParams(auto: AutoLinkParams, cgra: CGRAParams, packetCapacity: Int) {
  require(packetCapacity > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val wordBytes: Int = cgra.dataPayloadWidth / 8
}

case class CgraLinkAttachParams(adapter: CgraLinkParams, portName: String, controlAddress: BigInt, controlBytes: Int) {
  require(adapter.auto.endpoints.exists(_.name == portName))
}

case object CgraLinkKey extends Field[Option[CgraLinkAttachParams]](None)

class WithCgraLink(params: CgraLinkAttachParams) extends Config((_, _, _) => { case CgraLinkKey => Some(params) })

class CgraLinkConfig(params: CgraLinkParams) extends Bundle {
  val packetCount = UInt(params.packetCountWidth.W)
}

class CgraLinkConfigAck(params: CgraLinkParams) extends Bundle {
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
}

class CgraLinkDmaRequest(params: CgraLinkParams) extends Bundle {
  val sourceAddress = UInt(params.cgra.dma.dramAddrWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val bytes = UInt(params.auto.lengthWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkDmaCompletion(params: CgraLinkParams) extends Bundle {
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraLinkConfigAsync(params: CgraLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new CgraLinkConfig(params), crossing)
  val ack = Flipped(new AsyncBundle(new CgraLinkConfigAck(params), crossing))
}

/** Translates AutoLink copy and compute requests into CGRA traffic. */
class CgraLinkAdapter(params: CgraLinkParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraLinkConfig(params)))
    val configAck = Decoupled(new CgraLinkConfigAck(params))
    val packetIn = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val autoLink = new AutoEndpointIO(params.auto)
    val dmaRequest = Decoupled(new CgraLinkDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraLinkDmaCompletion(params)))
    val launchPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val computeResult = Flipped(Decoupled(UInt(params.auto.resultWidth.W)))
    val computeActive = Output(Bool())
  })

  object ConfigState {
    val idle :: collectPackets :: reportConfig :: holdConfig :: Nil = Enum(4)
  }
  object ExecState {
    val idle :: issueDma :: waitDma :: reportCopy :: sendPackets :: waitCompute :: reportCompute :: Nil = Enum(7)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new CgraLinkConfig(params))
  val copy = Reg(new AutoCopyRequest(params.auto))
  val packets = Reg(Vec(params.packetCapacity, UInt(params.cgra.intraPktWidth.W)))
  val configIndex = RegInit(0.U(params.packetCountWidth.W))
  val launchIndex = RegInit(0.U(params.packetCountWidth.W))
  val configStatus = RegInit(AutoLinkStatus.Success)
  val configDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copyDetail = RegInit(0.U(params.auto.detailWidth.W))
  val resultData = RegInit(0.U(params.auto.resultWidth.W))

  val configValid = io.configIn.bits.packetCount =/= 0.U &&
    io.configIn.bits.packetCount <= params.packetCapacity.U
  val packetCommand = io.packetIn.bits(
    params.cgra.packetLayout.cmdLsb + params.cgra.cmdWidth - 1,
    params.cgra.packetLayout.cmdLsb)
  val packetValid = packetCommand === CGRACmdGenerated.CMD_LAUNCH.U
  val configReady = configState === ConfigState.holdConfig
  val autoDmaTag = 0.U(params.cgra.dma.tagWidth.W)

  io.configIn.ready := configState === ConfigState.idle
  io.packetIn.ready := configState === ConfigState.collectPackets
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  io.autoLink.watchOutput.ready := false.B
  io.autoLink.reportOutput.valid := false.B
  io.autoLink.reportOutput.bits := 0.U.asTypeOf(new AutoEvent(params.auto))
  io.autoLink.requestCopy.ready := execState === ExecState.idle &&
    !io.autoLink.requestCompute.valid
  io.autoLink.reportCopy.valid := execState === ExecState.reportCopy
  io.autoLink.reportCopy.bits.task := copy.task
  io.autoLink.reportCopy.bits.status := Mux(
    copyDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SinkFailure)
  io.autoLink.reportCopy.bits.detail := copyDetail
  io.autoLink.requestCompute.ready := execState === ExecState.idle && configReady
  io.autoLink.reportCompute.valid := execState === ExecState.reportCompute
  io.autoLink.reportCompute.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCompute.bits.detail := 0.U
  io.autoLink.reportCompute.bits.data := resultData

  io.dmaRequest.valid := execState === ExecState.issueDma
  io.dmaRequest.bits.sourceAddress := copy.sourceAddress
  io.dmaRequest.bits.spmWordAddress :=
    copy.destinationOffset >> log2Ceil(params.wordBytes)
  io.dmaRequest.bits.bytes := copy.bytes
  io.dmaRequest.bits.dmaTag := autoDmaTag
  io.dmaCompletion.ready := execState === ExecState.waitDma

  io.launchPacket.valid := execState === ExecState.sendPackets
  io.launchPacket.bits := packets(launchIndex(params.packetIndexWidth - 1, 0))
  io.computeResult.ready := execState === ExecState.waitCompute
  io.computeActive := execState === ExecState.sendPackets ||
    execState === ExecState.waitCompute || execState === ExecState.reportCompute

  when(io.configIn.fire) {
    config := io.configIn.bits
    configIndex := 0.U
    when(configValid) {
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
      configState := ConfigState.collectPackets
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadConfig.U
      configState := ConfigState.reportConfig
    }
  }
  when(io.packetIn.fire) {
    when(packetValid) {
      packets(configIndex(params.packetIndexWidth - 1, 0)) := io.packetIn.bits
      when(configIndex + 1.U === config.packetCount) {
        configState := ConfigState.reportConfig
      }.otherwise {
        configIndex := configIndex + 1.U
      }
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadPacket.U
      configState := ConfigState.reportConfig
    }
  }
  when(io.configAck.fire) {
    configState := Mux(
      configStatus === AutoLinkStatus.Success,
      ConfigState.holdConfig,
      ConfigState.idle)
  }

  when(io.autoLink.requestCopy.fire) {
    copy := io.autoLink.requestCopy.bits
    copyDetail := 0.U
    execState := ExecState.issueDma
  }
  when(io.dmaRequest.fire) {
    execState := ExecState.waitDma
  }
  when(io.dmaCompletion.fire) {
    when(io.dmaCompletion.bits.dmaTag =/= autoDmaTag) {
      copyDetail := CgraLinkStatus.DmaMismatch.U
    }
    execState := ExecState.reportCopy
  }
  when(io.autoLink.reportCopy.fire) {
    execState := ExecState.idle
  }

  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      launchIndex := 0.U
      execState := ExecState.sendPackets
    }.otherwise {
      configState := ConfigState.idle
    }
  }
  when(io.launchPacket.fire) {
    when(launchIndex + 1.U === config.packetCount) {
      execState := ExecState.waitCompute
    }.otherwise {
      launchIndex := launchIndex + 1.U
    }
  }
  when(io.computeResult.fire) {
    resultData := io.computeResult.bits
    execState := ExecState.reportCompute
  }
  when(io.autoLink.reportCompute.fire) {
    configState := ConfigState.idle
    execState := ExecState.idle
  }
}

class CgraPacketArbiter(width: Int) extends Module {
  val io = IO(new Bundle {
    val dma = Flipped(Decoupled(UInt(width.W)))
    val cpu = Flipped(Decoupled(UInt(width.W)))
    val launch = Flipped(Decoupled(UInt(width.W)))
    val out = Decoupled(UInt(width.W))
  })

  val selected = RegInit(0.U(2.W))
  val locked = RegInit(false.B)
  val choice = Mux(locked, selected, Mux(io.dma.valid, 0.U, Mux(io.cpu.valid, 1.U, 2.U)))

  io.out.valid := MuxLookup(choice, false.B)(Seq(
    0.U -> io.dma.valid,
    1.U -> io.cpu.valid,
    2.U -> io.launch.valid))
  io.out.bits := MuxLookup(choice, 0.U)(Seq(
    0.U -> io.dma.bits,
    1.U -> io.cpu.bits,
    2.U -> io.launch.bits))
  io.dma.ready := io.out.ready && Mux(locked, selected === 0.U, true.B)
  io.cpu.ready := io.out.ready && Mux(locked, selected === 1.U, !io.dma.valid)
  io.launch.ready := io.out.ready && Mux(locked, selected === 2.U, !io.dma.valid && !io.cpu.valid)

  when(!locked && io.out.valid && !io.out.ready) {
    locked := true.B
    selected := choice
  }.elsewhen(locked && io.out.fire) {
    locked := false.B
  }
}
