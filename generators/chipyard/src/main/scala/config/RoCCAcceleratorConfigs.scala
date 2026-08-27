package chipyard

import chipyard.socgen.cgra.{CgraLinkAttachParams, CgraLinkParams, WithCgraLink}
import chipyard.socgen.config.AutoLinkExample
import chipyard.socgen.generated.{CgraLinkControlGenerated, CGRASpmWindowGenerated, GemminiExternalSpmGenerated}
import chipyard.socgen.gemmini.{GemminiLinkAttachParams, GemminiLinkParams, WithGemminiExternalSpm, WithGemminiExternalSpmWriter, WithGemminiLink}
import chipyard.socgen.link.WithAutoLink
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
    use_shared_ext_mem = false,
    use_tl_ext_mem = true,
    tl_ext_mem_base = externalSpm.baseAddress,
    sp_singleported = false,
    acc_sub_banks = 1)
}

class CGRAMinimalGemminiRocketConfig extends Config(
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new gemmini.DefaultGemminiConfig(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

class CGRAMinimalGemminiAESRocketConfig extends Config(
  new chipyard.example.WithCGRASpmWindow(CGRASpmWindowGenerated.params) ++
  new aes.WithAES256ECBAccel ++
  new WithGemminiExternalSpmWriter ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new gemmini.DefaultGemminiConfig(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiAutoLinkRocketConfig {
  private val example = AutoLinkExample

  val gemminiConfig = CGRAMinimalGemminiRocketConfig.minimalGemminiConfig
  private val writeBeatBytes =
    gemminiConfig.meshColumns * gemminiConfig.tileColumns * gemminiConfig.accType.getWidth / 8

  val autoLink = example.params
  val gemminiLink = GemminiLinkAttachParams(adapter = GemminiLinkParams(auto = autoLink, beatBytes = writeBeatBytes), portName = "gemmini")
  val cgraLink = CgraLinkAttachParams(
    adapter = CgraLinkParams(
      auto = autoLink,
      cgra = chipyard.example.CGRAGenerated.params,
      packetCapacity = 16),
    portName = "cgra",
    controlAddress = CgraLinkControlGenerated.baseAddress,
    controlBytes = CgraLinkControlGenerated.pageSizeBytes)
}

class CGRAMinimalGemminiAutoLinkRocketConfig extends Config(
  new WithCgraLink(CGRAMinimalGemminiAutoLinkRocketConfig.cgraLink) ++
  new WithGemminiLink(CGRAMinimalGemminiAutoLinkRocketConfig.gemminiLink) ++
  new WithAutoLink(CGRAMinimalGemminiAutoLinkRocketConfig.autoLink) ++
  new WithGemminiExternalSpm(CGRAMinimalGemminiRocketConfig.externalSpm) ++
  new chipyard.config.WithCGRA() ++
  new gemmini.DefaultGemminiConfig(
    CGRAMinimalGemminiAutoLinkRocketConfig.gemminiConfig) ++
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
