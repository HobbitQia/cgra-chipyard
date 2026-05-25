package chipyard

import org.chipsalliance.cde.config.{Config}

// ------------------------------
// Configs with RoCC Accelerators
// ------------------------------

// CGRA RoCC Accelerator Config (2x2 Mesh CGRA via custom0)
class CGRARocketConfig extends Config(
  new chipyard.config.WithCGRA() ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.AbstractConfig)

object CGRAMinimalGemminiRocketConfig {
  val minimalGemminiConfig:
    gemmini.GemminiArrayConfig[chisel3.SInt, gemmini.Float, gemmini.Float] =
    gemmini.GemminiConfigs.defaultConfig.copy(
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
    acc_read_full_width = false,
    acc_read_small_width = true,
    ex_read_from_acc = false,
    ex_write_to_spad = false,
    hardcode_d_to_garbage_addr = true,
    sp_capacity = gemmini.CapacityInKilobytes(64),
    acc_capacity = gemmini.CapacityInKilobytes(32),
    dma_maxbytes = 64,
    dma_buswidth = 128)
}

class CGRAMinimalGemminiRocketConfig extends Config(
  new chipyard.config.WithCGRA() ++
  new gemmini.DefaultGemminiConfig(CGRAMinimalGemminiRocketConfig.minimalGemminiConfig) ++
  new freechips.rocketchip.rocket.WithNBigCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
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
