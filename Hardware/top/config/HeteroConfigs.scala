package chipyard

import org.chipsalliance.cde.config.{Config}
import freechips.rocketchip.guardiancouncil._
import freechips.rocketchip.prci.{AsynchronousCrossing}
import freechips.rocketchip.subsystem.{InCluster}
import freechips.rocketchip.tile._
import peripheral._

// ---------------------
// Heterogenous Configs
// ---------------------
class LargeBoomAndRocketConfig extends Config(
  new boom.v3.common.WithNLargeBooms(1) ++                    // single-core boom
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++         // single rocket-core
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class DualLargeBoomAndDualRocketConfig extends Config(
  new boom.v3.common.WithNLargeBooms(2) ++             // add 2 boom cores
  new freechips.rocketchip.rocket.WithNHugeCores(2) ++  // add 2 rocket cores
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// DOC include start: DualBoomAndSingleRocket
class DualLargeBoomAndSingleRocketConfig extends Config(
  new boom.v3.common.WithNLargeBooms(2) ++             // add 2 boom cores
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++  // add 1 rocket core
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
// DOC include end: DualBoomAndSingleRocket

class LargeBoomAndRocketWithControlCoreConfig extends Config(
  new freechips.rocketchip.rocket.WithNSmallCores(1) ++    // Add a small "control" core
  new boom.v3.common.WithNLargeBooms(1) ++                 // Add 1 boom core
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++      // add 1 rocket core
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

/*
 * Voyager Config
 */

class GemminiPrefetchConfig extends Config(
  new barf.WithHellaCachePrefetcher(Seq(5), barf.SingleStridedPrefetcherParams()) ++   // strided prefetcher, sits in front of the L1D$, monitors core requests to prefetching into the L1D$
  new freechips.rocketchip.rocket.WithNBigNpuCores(1) ++ //independent Rocket for gemmini: hartid 5
  new chipyard.config.WithMultiRoCCNpu ++
  new chipyard.config.WithMultiRoCCGemmini(5)(gemmini.GemminiConfigs.defaultConfig) ++ // put gemmini on hart-5(rocket)
  new freechips.rocketchip.rocket.WithL1DCacheNonblocking(16) ++
  new chipyard.config.AbstractConfig
)


