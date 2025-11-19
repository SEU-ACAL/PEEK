package chipyard

import org.chipsalliance.cde.config.Config
import gemmini.{GemminiCustomConfig, GemminiCustomConfigs, GemminiFP16DefaultConfig}

class CustomGemminiSoCConfigFP16 extends Config(
  new gemmini.GemminiFP16DefaultConfig ++
  // new gemmini.GemminiFP32DefaultConfig ++
  // Set your custom L2 configs
  new chipyard.config.WithL2TLBs(512) ++
  // new chipyard.config.WithL2TLBs(8192) ++
  new freechips.rocketchip.subsystem.WithInclusiveCache(
    nWays = 8,
    capacityKB = 512,
    outerLatencyCycles = 40,
    subBankingFactor = 4
  ) ++
  new chipyard.CustomGemmminiCPUConfigs.CustomCPU(1) ++

  new chipyard.config.WithSystemBusWidth(GemminiCustomConfigs.customConfig.dma_buswidth) ++
  new chipyard.config.AbstractConfig
)
