package chipyard.socgen.gemmini

import chisel3._
import chisel3.util._
import chipyard.socgen.link._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.tilelink.TLMessages
import freechips.rocketchip.util.{AsyncBundle, AsyncQueueParams}
import org.chipsalliance.cde.config.Parameters

object GemminiLinkStatus {
  val BadAddress = 1
  val BadBeat = 2
  val BadOrder = 3
  val Denied = 4
  val Corrupt = 5
  val BadConfig = 6
}

case class GemminiLinkParams(auto: AutoLinkParams, beatBytes: Int, commandCapacity: Int) {
  require(isPow2(beatBytes))
  require(commandCapacity > 0)

  val commandCountWidth: Int = log2Ceil(commandCapacity + 1)
  val commandIndexWidth: Int = math.max(1, log2Ceil(commandCapacity))
}

class GemminiLinkWrite(params: GemminiLinkParams) extends Bundle {
  val address = UInt(64.W)
  val source = UInt(16.W)
  val size = UInt(8.W)
  val opcode = UInt(3.W)
  val mask = UInt(params.beatBytes.W)
}

class GemminiLinkAck extends Bundle {
  val source = UInt(16.W)
  val size = UInt(8.W)
  val denied = Bool()
  val corrupt = Bool()
}

class GemminiLinkEvent(params: GemminiLinkParams) extends Bundle {
  val isAck = Bool()
  val write = new GemminiLinkWrite(params)
  val ack = new GemminiLinkAck
}

class GemminiLinkConfig(params: GemminiLinkParams) extends Bundle {
  val commandCount = UInt(params.commandCountWidth.W)
}

class GemminiLinkConfigAck(params: GemminiLinkParams) extends Bundle {
  val status = UInt(AutoLinkStatus.Width.W)
  val detail = UInt(params.auto.detailWidth.W)
}

class GemminiLinkConfigAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val config = new AsyncBundle(new GemminiLinkConfig(params), crossing)
  val ack = Flipped(new AsyncBundle(new GemminiLinkConfigAck(params), crossing))
}

class GemminiLinkObserveAsync(params: GemminiLinkParams) extends Bundle {
  private val crossing = AsyncQueueParams.singleton()
  val event = new AsyncBundle(new GemminiLinkEvent(params), crossing)
}

/** Captures native CPU commands and replays them after AutoLink dependencies complete. */
class GemminiLinkAdapter(params: GemminiLinkParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val configIn = Flipped(Decoupled(new GemminiLinkConfig(params)))
    val configAck = Decoupled(new GemminiLinkConfigAck(params))
    val cpuCommand = Flipped(Decoupled(new RoCCCommand))
    val command = Decoupled(new RoCCCommand)
    val event = Flipped(Decoupled(new GemminiLinkEvent(params)))
    val autoLink = new AutoEndpointIO(params.auto)
    val autoBusy = Output(Bool())
  })

  object ConfigState {
    val idle :: reportConfig :: collect :: hold :: Nil = Enum(4)
  }
  object ExecState {
    val idle :: reportCopy :: waitCompute :: issue :: waitOutput :: reportCompute :: Nil = Enum(6)
  }

  val configState = RegInit(ConfigState.idle)
  val execState = RegInit(ExecState.idle)
  val config = Reg(new GemminiLinkConfig(params))
  val commands = Reg(Vec(params.commandCapacity, new RoCCCommand))
  val configIndex = RegInit(0.U(params.commandCountWidth.W))
  val commandIndex = RegInit(0.U(params.commandCountWidth.W))
  val configStatus = RegInit(AutoLinkStatus.Success)
  val configDetail = RegInit(0.U(params.auto.detailWidth.W))
  val copyTask = Reg(UInt(params.auto.taskWidth.W))
  val computeResult = Reg(new AutoEvent(params.auto))
  val watch = Reg(new AutoWatch(params.auto))
  val armed = RegInit(false.B)
  val active = RegInit(false.B)
  val issuedBytes = RegInit(0.U(params.auto.lengthWidth.W))
  val acknowledgedBytes = RegInit(0.U(params.auto.lengthWidth.W))
  val outstanding = RegInit(false.B)
  val source = Reg(UInt(16.W))
  val beatValid = RegInit(false.B)
  val beatError = RegInit(0.U(params.auto.detailWidth.W))
  val producedValid = RegInit(false.B)
  val producedDetail = RegInit(0.U(params.auto.detailWidth.W))
  val publication = Wire(Valid(new AutoEvent(params.auto)))

  val configValid = io.configIn.bits.commandCount =/= 0.U &&
    io.configIn.bits.commandCount <= params.commandCapacity.U
  val configReady = configState === ConfigState.hold
  val capture = configState === ConfigState.collect
  val replay = execState === ExecState.issue
  val expectedSize = log2Ceil(params.beatBytes).U
  val fullMask = ((BigInt(1) << params.beatBytes) - 1).U

  io.configIn.ready := configState === ConfigState.idle
  io.configAck.valid := configState === ConfigState.reportConfig
  io.configAck.bits.status := configStatus
  io.configAck.bits.detail := configDetail

  // Only one sequence is supported; extra commands may execute before the automatic job completes.
  io.cpuCommand.ready := Mux(capture, true.B, !replay && io.command.ready)
  io.command.valid := Mux(replay, true.B, io.cpuCommand.valid && !capture)
  io.command.bits := Mux(
    replay,
    commands(commandIndex(params.commandIndexWidth - 1, 0)),
    io.cpuCommand.bits)
  io.event.ready := true.B
  io.autoBusy := replay || execState === ExecState.waitOutput ||
    execState === ExecState.reportCompute

  io.autoLink.watchOutput.ready := !armed && !producedValid
  io.autoLink.reportOutput.valid := producedValid
  io.autoLink.reportOutput.bits.status := Mux(
    producedDetail === 0.U,
    AutoLinkStatus.Success,
    AutoLinkStatus.SourceFailure)
  io.autoLink.reportOutput.bits.detail := producedDetail
  io.autoLink.reportOutput.bits.data := 0.U
  io.autoLink.requestCopy.ready := execState === ExecState.idle && configReady
  io.autoLink.reportCopy.valid := execState === ExecState.reportCopy
  io.autoLink.reportCopy.bits.task := copyTask
  io.autoLink.reportCopy.bits.status := AutoLinkStatus.Success
  io.autoLink.reportCopy.bits.detail := 0.U
  io.autoLink.requestCompute.ready := Mux(
    io.autoLink.requestCompute.bits.start,
    execState === ExecState.waitCompute,
    execState === ExecState.idle || execState === ExecState.waitCompute)
  io.autoLink.reportCompute.valid := execState === ExecState.reportCompute
  io.autoLink.reportCompute.bits := computeResult
  publication.valid := false.B
  publication.bits := 0.U.asTypeOf(new AutoEvent(params.auto))

  def finish(detail: UInt): Unit = {
    producedValid := true.B
    producedDetail := detail
    publication.valid := true.B
    publication.bits.status := Mux(
      detail === 0.U,
      AutoLinkStatus.Success,
      AutoLinkStatus.SourceFailure)
    publication.bits.detail := detail
    publication.bits.data := 0.U
  }

  when(io.configIn.fire) {
    config := io.configIn.bits
    configIndex := 0.U
    when(configValid) {
      configStatus := AutoLinkStatus.Success
      configDetail := 0.U
    }.otherwise {
      configStatus := AutoLinkStatus.SinkFailure
      configDetail := GemminiLinkStatus.BadConfig.U
    }
    configState := ConfigState.reportConfig
  }
  when(io.configAck.fire) {
    configState := Mux(
      configStatus === AutoLinkStatus.Success,
      ConfigState.collect,
      ConfigState.idle)
  }
  when(capture && io.cpuCommand.fire) {
    commands(configIndex(params.commandIndexWidth - 1, 0)) := io.cpuCommand.bits
    when(configIndex + 1.U === config.commandCount) {
      configState := ConfigState.hold
    }.otherwise {
      configIndex := configIndex + 1.U
    }
  }

  when(io.autoLink.requestCopy.fire) {
    copyTask := io.autoLink.requestCopy.bits.task
    execState := ExecState.reportCopy
  }
  when(io.autoLink.reportCopy.fire) {
    execState := ExecState.waitCompute
  }
  when(io.autoLink.requestCompute.fire) {
    when(io.autoLink.requestCompute.bits.start) {
      commandIndex := 0.U
      execState := ExecState.issue
    }.otherwise {
      configState := ConfigState.idle
      execState := ExecState.idle
    }
  }
  when(replay && io.command.fire) {
    when(commandIndex + 1.U === config.commandCount) {
      execState := ExecState.waitOutput
    }.otherwise {
      commandIndex := commandIndex + 1.U
    }
  }
  when(execState === ExecState.waitOutput && publication.valid) {
    computeResult := publication.bits
    execState := ExecState.reportCompute
  }
  when(io.autoLink.reportCompute.fire) {
    configState := ConfigState.idle
    execState := ExecState.idle
  }

  when(io.autoLink.watchOutput.fire) {
    watch := io.autoLink.watchOutput.bits
    armed := true.B
    active := false.B
    issuedBytes := 0.U
    acknowledgedBytes := 0.U
    outstanding := false.B
  }

  when(armed && !producedValid && io.event.fire && !io.event.bits.isAck) {
    val write = io.event.bits.write
    val expectedAddress = watch.address + issuedBytes
    val addressValid = write.address === expectedAddress
    val shapeValid = write.opcode === TLMessages.PutFullData &&
      write.size === expectedSize && write.mask === fullMask
    val withinPublication = issuedBytes < watch.bytes

    active := true.B
    when(outstanding) {
      beatValid := false.B
      beatError := GemminiLinkStatus.BadOrder.U
    }.otherwise {
      outstanding := true.B
      source := write.source
      beatValid := addressValid && shapeValid && withinPublication
      beatError := Mux(
        !addressValid,
        GemminiLinkStatus.BadAddress.U,
        Mux(!shapeValid, GemminiLinkStatus.BadBeat.U, GemminiLinkStatus.BadOrder.U))
      when(addressValid && shapeValid && withinPublication) {
        issuedBytes := issuedBytes + params.beatBytes.U
      }
    }
  }

  when(active && !producedValid && io.event.fire && io.event.bits.isAck) {
    val ack = io.event.bits.ack
    when(!outstanding) {
      finish(GemminiLinkStatus.BadOrder.U)
    }.otherwise {
      val responseShapeValid = ack.source === source &&
        ack.size === expectedSize
      val responseValid = beatValid && responseShapeValid &&
        !ack.denied && !ack.corrupt
      val detail = Mux(
        ack.denied,
        GemminiLinkStatus.Denied.U,
        Mux(
          ack.corrupt,
          GemminiLinkStatus.Corrupt.U,
          Mux(!responseShapeValid, GemminiLinkStatus.BadBeat.U, beatError)))
      val nextBytes = Mux(
        responseValid,
        acknowledgedBytes + params.beatBytes.U,
        acknowledgedBytes)

      acknowledgedBytes := nextBytes
      outstanding := false.B
      when(!responseValid) {
        finish(detail)
      }.elsewhen(nextBytes === watch.bytes) {
        finish(0.U)
      }
    }
  }

  when(io.autoLink.reportOutput.fire) {
    armed := false.B
    active := false.B
    outstanding := false.B
    producedValid := false.B
  }
}
