package chipyard.example

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}

object CgraSpmConfigKind {
  val Width = 1
  val Header = 0.U(Width.W)
  val Packet = 1.U(Width.W)
}

object CgraSpmStatus {
  val BadConfig = 1
  val BadPacket = 2
  val BadDelivery = 3
  val DmaMismatch = 4
}

case class CgraSpmParams(
  link: SpmLinkParams,
  table: SpmCommunicationTable,
  cgra: CGRAParams,
  slotBases: Seq[BigInt],
  packetCapacity: Int) {
  table.validate(link)
  require(table.waitFor.size == 1)
  require(table.publishTo.isEmpty)
  require(slotBases.size == link.slotCount)
  require(packetCapacity > 0)

  val delivery: SpmCommunicationRule = table.waitFor.head
  val packetCountWidth: Int = log2Ceil(packetCapacity + 1)
  val packetIndexWidth: Int = math.max(1, log2Ceil(packetCapacity))
  val wordBytes: Int = cgra.dataPayloadWidth / 8
  val spmBytes: Int = cgra.dma.spmWords * wordBytes
  val dmaBeatBytes: Int = cgra.dma.dramDataWidth / 8
  require(delivery.bytes <= spmBytes)
  require(delivery.bytes % wordBytes == 0)
  require(delivery.bytes % dmaBeatBytes == 0)
}

class CgraSpmHeader(params: CgraSpmParams) extends Bundle {
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
  val packetCount = UInt(params.packetCountWidth.W)
}

class CgraSpmConfig(params: CgraSpmParams) extends Bundle {
  val kind = UInt(CgraSpmConfigKind.Width.W)
  val header = new CgraSpmHeader(params)
  val packet = UInt(params.cgra.intraPktWidth.W)
}

class CgraSpmConfigAck(params: CgraSpmParams) extends Bundle {
  val status = UInt(SpmLinkStatus.Width.W)
  val detail = UInt(params.link.detailWidth.W)
}

class CgraSpmDmaRequest(params: CgraSpmParams) extends Bundle {
  val sourceAddress = UInt(params.cgra.dma.dramAddrWidth.W)
  val spmWordAddress = UInt(params.cgra.dma.spmAddrWidth.W)
  val bytes = UInt(params.link.lengthWidth.W)
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraSpmDmaCompletion(params: CgraSpmParams) extends Bundle {
  val dmaTag = UInt(params.cgra.dma.tagWidth.W)
}

class CgraSpmAsyncLink(params: CgraSpmParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new CgraSpmConfig(params), crossing)
  val configAck = Flipped(new AsyncBundle(new CgraSpmConfigAck(params), crossing))
  val deliver = new AsyncBundle(new SpmLinkEvent(params.link), crossing)
  val done = Flipped(new AsyncBundle(new SpmLinkEvent(params.link), crossing))
}

/** Imports one Shared SPM publication and starts the configured CGRA kernel. */
class CgraSpmAdapter(params: CgraSpmParams) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new CgraSpmConfig(params)))
    val configAck = Decoupled(new CgraSpmConfigAck(params))
    val endpoint = new SpmEndpointIO(params.link)
    val dmaRequest = Decoupled(new CgraSpmDmaRequest(params))
    val dmaCompletion = Flipped(Decoupled(new CgraSpmDmaCompletion(params)))
    val launchPacket = Decoupled(UInt(params.cgra.intraPktWidth.W))
    val complete = Flipped(Decoupled(UInt(params.link.resultWidth.W)))
    val cpuComputeActive = Input(Bool())
    val active = Output(Bool())
    val computeActive = Output(Bool())
  })

  val Seq(empty, collect, configResult, armed, issueDma, waitDma, launch, waitCompute, result) = Enum(9)
  val state = RegInit(empty)
  val header = Reg(new CgraSpmHeader(params))
  val delivery = Reg(new SpmLinkEvent(params.link))
  val packets = Reg(Vec(params.packetCapacity, UInt(params.cgra.intraPktWidth.W)))
  val packetIndex = RegInit(0.U(params.packetCountWidth.W))
  val configStatus = RegInit(SpmLinkStatus.Success)
  val configDetail = RegInit(0.U(params.link.detailWidth.W))
  val resultDetail = RegInit(0.U(params.link.detailWidth.W))
  val resultData = RegInit(0.U(params.link.resultWidth.W))

  val deliveryWords = params.delivery.bytes >> log2Ceil(params.wordBytes)
  val headerEnd = io.configIn.bits.header.spmWordAddress +& deliveryWords.U
  val headerValid = headerEnd <= params.cgra.dma.spmWords.U &&
    io.configIn.bits.header.packetCount =/= 0.U &&
    io.configIn.bits.header.packetCount <= params.packetCapacity.U
  val packetCommand = io.configIn.bits.packet(
    params.cgra.packetLayout.cmdLsb + params.cgra.cmdWidth - 1,
    params.cgra.packetLayout.cmdLsb)
  val packetValid = io.configIn.bits.kind === CgraSpmConfigKind.Packet &&
    packetCommand === CGRACmdGenerated.CMD_LAUNCH.U

  io.configIn.ready := state === empty || state === collect
  io.configAck.valid := state === configResult &&
    (configStatus =/= SpmLinkStatus.Success || !io.cpuComputeActive)
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  io.endpoint.produced.valid := false.B
  io.endpoint.produced.bits := 0.U.asTypeOf(new SpmLinkEvent(params.link))
  io.endpoint.deliver.ready := state === armed
  io.endpoint.done.valid := state === result
  io.endpoint.done.bits := delivery
  io.endpoint.done.bits.status := Mux(
    resultDetail === 0.U,
    SpmLinkStatus.Success,
    SpmLinkStatus.SinkFailure)
  io.endpoint.done.bits.detail := resultDetail
  io.endpoint.done.bits.data := resultData

  io.dmaRequest.valid := state === issueDma
  io.dmaRequest.bits.sourceAddress := VecInit(
    params.slotBases.map(_.U(params.cgra.dma.dramAddrWidth.W)))(delivery.slot)
  io.dmaRequest.bits.spmWordAddress := header.spmWordAddress
  io.dmaRequest.bits.bytes := delivery.bytes
  io.dmaRequest.bits.dmaTag := header.dmaTag
  io.dmaCompletion.ready := state === waitDma

  io.launchPacket.valid := state === launch
  io.launchPacket.bits := packets(packetIndex(params.packetIndexWidth - 1, 0))
  io.complete.ready := state === waitCompute
  io.active := state =/= empty
  io.computeActive := state === launch || state === waitCompute || state === result

  when(io.configIn.fire) {
    when(state === empty) {
      header := io.configIn.bits.header
      packetIndex := 0.U
      when(io.configIn.bits.kind =/= CgraSpmConfigKind.Header || !headerValid) {
        configStatus := SpmLinkStatus.SinkFailure
        configDetail := CgraSpmStatus.BadConfig.U
        state := configResult
      }.otherwise {
        configStatus := SpmLinkStatus.Success
        configDetail := 0.U
        state := collect
      }
    }.otherwise {
      when(!packetValid) {
        configStatus := SpmLinkStatus.SinkFailure
        configDetail := CgraSpmStatus.BadPacket.U
        state := configResult
      }.otherwise {
        packets(packetIndex(params.packetIndexWidth - 1, 0)) := io.configIn.bits.packet
        when(packetIndex + 1.U === header.packetCount) {
          state := configResult
        }.otherwise {
          packetIndex := packetIndex + 1.U
        }
      }
    }
  }

  when(io.configAck.fire) {
    state := Mux(configStatus === SpmLinkStatus.Success, armed, empty)
  }

  when(io.endpoint.deliver.fire) {
    delivery := io.endpoint.deliver.bits
    resultData := 0.U
    when(io.endpoint.deliver.bits.link =/= params.delivery.link.U ||
      io.endpoint.deliver.bits.slot =/= params.delivery.slot.U ||
      io.endpoint.deliver.bits.bytes =/= params.delivery.bytes.U ||
      io.endpoint.deliver.bits.status =/= SpmLinkStatus.Success) {
      resultDetail := CgraSpmStatus.BadDelivery.U
      state := result
    }.otherwise {
      resultDetail := 0.U
      state := issueDma
    }
  }

  when(io.dmaRequest.fire) {
    state := waitDma
  }

  when(io.dmaCompletion.fire) {
    when(io.dmaCompletion.bits.dmaTag === header.dmaTag) {
      packetIndex := 0.U
      state := launch
    }.otherwise {
      resultDetail := CgraSpmStatus.DmaMismatch.U
      state := result
    }
  }

  when(io.launchPacket.fire) {
    when(packetIndex + 1.U === header.packetCount) {
      state := waitCompute
    }.otherwise {
      packetIndex := packetIndex + 1.U
    }
  }

  when(io.complete.fire) {
    resultData := io.complete.bits
    state := result
  }

  when(io.endpoint.done.fire) {
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
