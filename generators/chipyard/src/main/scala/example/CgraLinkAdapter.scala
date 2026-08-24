package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.{Config, Field}

object CgraLinkStatus {
  val BadConfig = 1
  val BadPacket = 2
  val DmaMismatch = 3
}

case class CgraLinkParams(
  auto: AutoLinkParams,
  endpoint: String,
  cgra: CGRAParams,
  packetCapacity: Int) {
  require(auto.endpoints.exists(_.name == endpoint))
  require(packetCapacity > 0)

  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val wordBytes: Int = cgra.dataPayloadWidth / 8
}

case class CgraLinkAttachParams(
  adapter: CgraLinkParams,
  controlAddress: BigInt,
  controlBytes: Int)

case object CgraLinkKey extends Field[Option[CgraLinkAttachParams]](None)

class WithCgraLink(params: CgraLinkAttachParams)
    extends Config((_, _, _) => { case CgraLinkKey => Some(params) })

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

/** Translates AutoLink transfers and releases into CGRA DMA and launch traffic. */
class CgraLinkAdapter(params: CgraLinkParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraLinkConfig(params)))
    val configAck = Decoupled(new CgraLinkConfigAck(params))
    val packetIn = Flipped(Decoupled(UInt(params.cgra.intraPktWidth.W)))
    val endpoint = new AutoEndpointIO(params.auto)
    val dmaRequest = Decoupled(new CgraLinkDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraLinkDmaCompletion(params)))
    val launchPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val computeResult = Flipped(Decoupled(UInt(params.auto.resultWidth.W)))
    val computeActive = Output(Bool())
  })

  val Seq(
    empty,
    collect,
    configResult,
    armed,
    issueDma,
    waitDma,
    reportTransfer,
    launch,
    waitCompute,
    reportComplete) = Enum(10)
  val state = RegInit(empty)
  val config = Reg(new CgraLinkConfig(params))
  val transfer = Reg(new AutoTransfer(params.auto))
  val packets = Reg(Vec(params.packetCapacity, UInt(params.cgra.intraPktWidth.W)))
  val packetIndex = RegInit(0.U(params.packetCountWidth.W))
  val configStatus = RegInit(AutoLinkStatus.Success)
  val configDetail = RegInit(0.U(params.auto.detailWidth.W))
  val transferDetail = RegInit(0.U(params.auto.detailWidth.W))
  val resultData = RegInit(0.U(params.auto.resultWidth.W))

  val configValid = io.configIn.bits.packetCount =/= 0.U &&
    io.configIn.bits.packetCount <= params.packetCapacity.U
  val packetCommand = io.packetIn.bits(
    params.cgra.packetLayout.cmdLsb + params.cgra.cmdWidth - 1,
    params.cgra.packetLayout.cmdLsb)
  val packetValid = packetCommand === CGRACmdGenerated.CMD_LAUNCH.U
  val autoDmaTag = 0.U(params.cgra.dma.tagWidth.W)

  io.configIn.ready := state === empty
  io.packetIn.ready := state === collect
  io.configAck.valid := state === configResult
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  io.endpoint.watch.ready := false.B
  io.endpoint.produced.valid := false.B
  io.endpoint.produced.bits := 0.U.asTypeOf(new AutoEvent(params.auto))
  io.endpoint.transfer.ready := state === armed
  io.endpoint.transferred.valid := state === reportTransfer
  io.endpoint.transferred.bits.task := transfer.task
  io.endpoint.transferred.bits.status := Mux(
    transferDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SinkFailure)
  io.endpoint.transferred.bits.detail := transferDetail
  io.endpoint.release.ready := state === armed
  io.endpoint.complete.valid := state === reportComplete
  io.endpoint.complete.bits.status := AutoLinkStatus.Success
  io.endpoint.complete.bits.detail := 0.U
  io.endpoint.complete.bits.data := resultData

  io.dmaRequest.valid := state === issueDma
  io.dmaRequest.bits.sourceAddress := transfer.sourceAddress
  io.dmaRequest.bits.spmWordAddress :=
    transfer.destinationOffset >> log2Ceil(params.wordBytes)
  io.dmaRequest.bits.bytes := transfer.bytes
  io.dmaRequest.bits.dmaTag := autoDmaTag
  io.dmaCompletion.ready := state === waitDma

  io.launchPacket.valid := state === launch
  io.launchPacket.bits := packets(packetIndex(params.packetIndexWidth - 1, 0))
  io.computeResult.ready := state === waitCompute
  io.computeActive := state === launch || state === waitCompute ||
    state === reportComplete

  when(io.configIn.fire) {
    config := io.configIn.bits
    packetIndex := 0.U
    when(configValid) {
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
      state := collect
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadConfig.U
      state := configResult
    }
  }

  when(io.packetIn.fire) {
    when(packetValid) {
      packets(packetIndex(params.packetIndexWidth - 1, 0)) := io.packetIn.bits
      when(packetIndex + 1.U === config.packetCount) {
        state := configResult
      }.otherwise {
        packetIndex := packetIndex + 1.U
      }
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := CgraLinkStatus.BadPacket.U
      state := configResult
    }
  }

  when(io.configAck.fire) {
    state := Mux(configStatus === AutoLinkStatus.Success, armed, empty)
  }

  when(io.endpoint.transfer.fire) {
    transfer := io.endpoint.transfer.bits
    transferDetail := 0.U
    state := issueDma
  }
  when(io.dmaRequest.fire) {
    state := waitDma
  }
  when(io.dmaCompletion.fire) {
    when(io.dmaCompletion.bits.dmaTag =/= autoDmaTag) {
      transferDetail := CgraLinkStatus.DmaMismatch.U
    }
    state := reportTransfer
  }
  when(io.endpoint.transferred.fire) {
    state := armed
  }

  when(io.endpoint.release.fire) {
    when(io.endpoint.release.bits.start) {
      packetIndex := 0.U
      state := launch
    }.otherwise {
      state := empty
    }
  }
  when(io.launchPacket.fire) {
    when(packetIndex + 1.U === config.packetCount) {
      state := waitCompute
    }.otherwise {
      packetIndex := packetIndex + 1.U
    }
  }
  when(io.computeResult.fire) {
    resultData := io.computeResult.bits
    state := reportComplete
  }
  when(io.endpoint.complete.fire) {
    state := empty
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
