package chipyard

import chipyard.socgen.aes.{AesLinkAttachParams, AesLinkParams, WithAesLink}
import chipyard.socgen.cgra.{CgraLinkAttachParams, CgraLinkParams, WithCgraLink}
import chipyard.socgen.generated.{AutoLinkGenerated, CgraLinkControlGenerated, CGRASpmWindowGenerated, GemminiExternalSpmGenerated}
import chipyard.socgen.gemmini.{GemminiLinkAttachParams, GemminiLinkParams, WithGemminiExternalSpm, WithGemminiExternalSpmWriter, WithGemminiLink, WithGemminiRoCC}
import chipyard.socgen.link.WithAutoLink
import chipyard.socgen.pool.{PoolLinkAttachParams, PoolLinkParams, PoolParams, WithPoolAccelerator, WithPoolLink}
import org.chipsalliance.cde.config.{Config}

// ------------------------------
// Configs with RoCC Accelerators
// ------------------------------

// CGRA RoCC Accelerator Config (2x2 Mesh CGRA via custom0)
class CGRARocketConfig extends Config(
  new chipyard.config.WithCGRA() ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiRocketConfig {
  val externalSpm = GemminiExternalSpmGenerated.params
  val minimalGemminiConfig: gemmini.GemminiArrayConfig[chisel3.SInt, gemmini.Float, gemmini.Float] = gemmini.GemminiConfigs.defaultConfig.copy(
    dataflow = gemmini.Dataflow.WS,
    has_training_convs = false,
    has_max_pool = false,
    has_nonlinear_activations = false,
    has_dw_convs = false,
    has_normalizations = false,
    has_first_layer_optimizations = false,
    has_loop_conv = false,
    mvin_scale_args = None,
    mvin_scale_acc_args = None,
    mvin_scale_shared = false,
    acc_scale_args = None,
    acc_read_full_width = true,
    acc_read_small_width = true,
    ex_read_from_acc = false,
    ex_write_to_spad = false,
    hardcode_d_to_garbage_addr = true,
    sp_capacity = gemmini.CapacityInKilobytes(64),
    acc_capacity = gemmini.CapacityInKilobytes(32),
    dma_maxbytes = 64,
    dma_buswidth = 256,
    use_shared_ext_mem = true,
    use_tl_ext_mem = true,
    tl_ext_mem_base = externalSpm.baseAddress,
    sp_singleported = false,
    acc_sub_banks = 1)
}

class CGRAMinimalGemminiRocketConfig extends Config(
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiAESRocketConfig extends Config(
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new aes.WithAES256ECBAccel ++
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiAutoLinkRocketConfig {
  val gemminiConfig = CGRAMinimalGemminiRocketConfig.minimalGemminiConfig
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8

  val autoLink = AutoLinkGenerated.params
  val gemminiLink = GemminiLinkAttachParams(
    adapter = GemminiLinkParams(
      auto = autoLink,
      beatBytes = writeBeatBytes,
      commandCapacity = 16,
      maxInflight = gemminiConfig.max_in_flight_mem_reqs *
        (1 + gemminiConfig.dma_maxbytes / (gemminiConfig.dma_buswidth / 8))),
    portName = "gemmini")
  val cgraLink = CgraLinkAttachParams(
    adapter = CgraLinkParams(
      auto = autoLink,
      cgra = chipyard.example.CGRAGenerated.params,
      packetCapacity = 96),
    portName = "cgra",
    resultNames = autoLink.resultNames,
    controlAddress = CgraLinkControlGenerated.baseAddress,
    controlBytes = CgraLinkControlGenerated.pageSizeBytes)
}

class CGRAMinimalGemminiAutoLinkRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiAutoLinkRocketConfig.cgraLink) ++
  new WithGemminiLink(CGRAMinimalGemminiAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiAutoLinkRocketConfig.autoLink) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiAutoLinkRocketConfig.gemminiConfig,
    linkParams = Some(CGRAMinimalGemminiAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CgraConvManualRocketConfig extends Config(
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiRocketConfig.minimalGemminiConfig.copy(has_loop_conv = true)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CgraConvRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiAutoLinkRocketConfig.cgraLink) ++
  new WithGemminiLink(CGRAMinimalGemminiAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiAutoLinkRocketConfig.autoLink) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiAutoLinkRocketConfig.gemminiConfig.copy(has_loop_conv = true),
    linkParams = Some(CGRAMinimalGemminiAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiAESAutoLinkRocketConfig {
  val gemminiConfig = CGRAMinimalGemminiRocketConfig.minimalGemminiConfig
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8

  val autoLink = AutoLinkGenerated.params
  val gemminiLink = GemminiLinkAttachParams(
    adapter = GemminiLinkParams(
      auto = autoLink,
      beatBytes = writeBeatBytes,
      commandCapacity = 16,
      maxInflight = gemminiConfig.max_in_flight_mem_reqs *
        (1 + gemminiConfig.dma_maxbytes / (gemminiConfig.dma_buswidth / 8))),
    portName = "gemmini")
  val cgraLink = CgraLinkAttachParams(
    adapter = CgraLinkParams(
      auto = autoLink,
      cgra = chipyard.example.CGRAGenerated.params,
      packetCapacity = 96),
    portName = "cgra",
    resultNames = autoLink.resultNames,
    controlAddress = CgraLinkControlGenerated.baseAddress,
    controlBytes = CgraLinkControlGenerated.pageSizeBytes)
  val aesLink = AesLinkAttachParams(
    adapter = AesLinkParams(auto = autoLink),
    portName = "aes")
}

class CGRAMinimalGemminiAESAutoLinkRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiAESAutoLinkRocketConfig.cgraLink) ++
  new WithAesLink(CGRAMinimalGemminiAESAutoLinkRocketConfig.aesLink) ++
  new WithGemminiLink(CGRAMinimalGemminiAESAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiAESAutoLinkRocketConfig.autoLink) ++
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new aes.WithAESJobPort ++
  new aes.WithAES256ECBAccel ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiAESAutoLinkRocketConfig.gemminiConfig,
    linkParams = Some(CGRAMinimalGemminiAESAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiPoolRocketConfig extends Config(
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithPoolAccelerator(PoolParams(elementBits = 32)) ++
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiPoolAutoLinkRocketConfig {
  val gemminiConfig = CGRAMinimalGemminiRocketConfig.minimalGemminiConfig
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8

  val autoLink = AutoLinkGenerated.params
  val gemminiLink = GemminiLinkAttachParams(
    adapter = GemminiLinkParams(
      auto = autoLink,
      beatBytes = writeBeatBytes,
      commandCapacity = 16,
      maxInflight = gemminiConfig.max_in_flight_mem_reqs *
        (1 + gemminiConfig.dma_maxbytes / (gemminiConfig.dma_buswidth / 8))),
    portName = "gemmini")
  val cgraLink = CgraLinkAttachParams(
    adapter = CgraLinkParams(
      auto = autoLink,
      cgra = chipyard.example.CGRAGenerated.params,
      packetCapacity = 96),
    portName = "cgra",
    resultNames = autoLink.resultNames,
    controlAddress = CgraLinkControlGenerated.baseAddress,
    controlBytes = CgraLinkControlGenerated.pageSizeBytes)
  val poolLink = PoolLinkAttachParams(
    adapter = PoolLinkParams(autoLink),
    portName = "pool")
}

class CgraPoolTileRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.cgraLink) ++
  new WithPoolLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.poolLink) ++
  new WithGemminiLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.autoLink) ++
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithPoolAccelerator(PoolParams(elementBits = 8)) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiConfig.copy(has_loop_conv = true),
    linkParams = Some(CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiPoolAutoLinkRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.cgraLink) ++
  new WithPoolLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.poolLink) ++
  new WithGemminiLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiPoolAutoLinkRocketConfig.autoLink) ++
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithPoolAccelerator(PoolParams(elementBits = 32)) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiConfig,
    linkParams = Some(CGRAMinimalGemminiPoolAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiResidualRocketConfig extends Config(
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiResidualAutoLinkRocketConfig {
  val gemminiConfig = CGRAMinimalGemminiRocketConfig.minimalGemminiConfig
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8

  val autoLink = AutoLinkGenerated.params
  val gemminiLink = GemminiLinkAttachParams(
    adapter = GemminiLinkParams(
      auto = autoLink,
      beatBytes = writeBeatBytes,
      commandCapacity = 7,
      maxInflight = gemminiConfig.max_in_flight_mem_reqs *
        (1 + gemminiConfig.dma_maxbytes / (gemminiConfig.dma_buswidth / 8))),
    portName = "gemmini")
  val cgraLink = CgraLinkAttachParams(
    adapter = CgraLinkParams(
      auto = autoLink,
      cgra = chipyard.example.CGRAGenerated.params,
      packetCapacity = 96),
    portName = "cgra",
    resultNames = autoLink.resultNames,
    controlAddress = CgraLinkControlGenerated.baseAddress,
    controlBytes = CgraLinkControlGenerated.pageSizeBytes)
}

object CgraResidualTileRocketConfig {
  private val base = CGRAMinimalGemminiResidualAutoLinkRocketConfig.gemminiLink
  val gemminiLink = base.copy(adapter = base.adapter.copy(commandCapacity = 16))
}

class CgraResidualTileRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiResidualAutoLinkRocketConfig.cgraLink) ++
  new WithGemminiLink(CgraResidualTileRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiResidualAutoLinkRocketConfig.autoLink) ++
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiResidualAutoLinkRocketConfig.gemminiConfig.copy(has_loop_conv = true),
    linkParams = Some(CgraResidualTileRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiResidualAutoLinkRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiResidualAutoLinkRocketConfig.cgraLink) ++
  new WithGemminiLink(CGRAMinimalGemminiResidualAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiResidualAutoLinkRocketConfig.autoLink) ++
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new WithGemminiRoCC(
    CGRAMinimalGemminiResidualAutoLinkRocketConfig.gemminiConfig,
    linkParams = Some(CGRAMinimalGemminiResidualAutoLinkRocketConfig.gemminiLink.adapter)) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class ReRoCCTestConfig extends Config(
  new rerocc.WithReRoCC ++
  new chipyard.config.WithCharacterCountRoCC ++                // rerocc tile4 is charcnt
  new chipyard.config.WithAccumulatorRoCC ++                   // rerocc tile3 is accum
  new chipyard.config.WithAccumulatorRoCC ++                   // rerocc tile2 is accum
  new chipyard.config.WithAccumulatorRoCC ++                   // rerocc tile1 is accum
  new chipyard.config.WithAccumulatorRoCC ++                   // rerocc tile0 is accum
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.AbstractConfig)
