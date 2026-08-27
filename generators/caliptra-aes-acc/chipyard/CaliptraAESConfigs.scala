package chipyard

import org.chipsalliance.cde.config.{Config}

class AES256ECBRocketConfig extends Config(
  new aes.WithAES256ECBAccel ++                                   // use Caliptra AES 256 ECB accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)
