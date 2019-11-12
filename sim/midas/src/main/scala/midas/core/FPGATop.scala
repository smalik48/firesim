// See LICENSE for license details.

package midas
package core

import junctions._
import midas.widgets._
import chisel3._
import chisel3.util._
import chisel3.core.ActualDirection
import chisel3.core.DataMirror.directionOf
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{DecoupledHelper, HeterogeneousBag}

import scala.collection.mutable

case object DMANastiKey extends Field[NastiParameters]
case object FpgaMMIOSize extends Field[BigInt]

// The AXI4 widths for a single host-DRAM channel
case object HostMemChannelKey extends Field[HostMemChannelParams]
// The number of host-DRAM channels -> all channels must have the same AXI4 widths
case object HostMemNumChannels extends Field[Int]
// The aggregate memory-space seen by masters wanting DRAM
case object MemNastiKey extends Field[NastiParameters]

class FPGATopIO(implicit val p: Parameters) extends WidgetIO {
  val dma  = Flipped(new NastiIO()(p alterPartial ({ case NastiKey => p(DMANastiKey) })))
}

/** Specifies the size and width of external memory ports */
case class HostMemChannelParams(
  size: BigInt,
  beatBytes: Int,
  idBits: Int,
  maxXferBytes: Int = 256)

/** Specifies the width of external slave ports */

// Platform agnostic wrapper of the simulation models for FPGA
class FPGATop(implicit p: Parameters) extends LazyModule with HasWidgets {
  val headerConsts = new mutable.ArrayBuffer[(String, Long)]
  val SimWrapperConfig(chAnnos, bridgeAnnos, leafTypeMap) = p(SimWrapperKey)
  val master = addWidget(new SimulationMaster)
  val loadMem = addWidget(new LoadMemWidget)
  val bridgeModuleMap: Map[BridgeIOAnnotation, BridgeModule[_ <: TokenizedRecord]] = bridgeAnnos.map(anno => anno -> addWidget(anno.elaborateWidget)).toMap
  val bridgesRequiringDRAM = bridgeModuleMap.values.collect({ case b: RequiresHostDRAM =>  b})

  // Host DRAM handling
  val memChannelParams = p(HostMemChannelKey)
  val memAXI4Node = AXI4SlaveNode(Seq.tabulate(p(HostMemNumChannels)) { channel =>
    val device = new MemoryDevice
    val base = channel * memChannelParams.size
    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address       = Seq(AddressSet(base, memChannelParams.size - 1)),
        resources     = device.reg,
        regionType    = RegionType.UNCACHED, // cacheable
        executable    = false,
        supportsWrite = TransferSizes(1, memChannelParams.maxXferBytes),
        supportsRead  = TransferSizes(1, memChannelParams.maxXferBytes),
        interleavedId = Some(0))), // slave does not interleave read responses
      beatBytes = memChannelParams.beatBytes)
  })

  val xbar = AXI4Xbar()
  memAXI4Node :*= AXI4Buffer() :*= xbar
  xbar := loadMem.toHostMemory
  bridgesRequiringDRAM.foreach(bridge => xbar := AXI4Buffer() := bridge.hostDRAMMasterNode)

  def hostMemoryBundleParams(): AXI4BundleParameters = memAXI4Node.in(0)._1.params

  lazy val module = new FPGATopImp(this)
}

class FPGATopImp(wrapper: FPGATop)(implicit p: Parameters) extends LazyModuleImp(wrapper) {

  val loadMem = wrapper.loadMem
  val master  = wrapper.master

  val io = IO(new FPGATopIO)
  val mem = IO(HeterogeneousBag.fromNode(wrapper.memAXI4Node.in))
  (mem zip wrapper.memAXI4Node.in).foreach({ case (io, (bundle, _)) => io <> bundle })

  val sim = Module(new SimWrapper(p(SimWrapperKey)))
  val simIo = sim.channelPorts
  // This reset is used to return the simulation to time 0.
  val simReset = master.module.io.simReset

  sim.clock     := clock
  sim.reset     := reset.toBool || simReset
  sim.hostReset := simReset

  case class DmaInfo(name: String, port: NastiIO, size: BigInt)
  val dmaInfoBuffer = new mutable.ListBuffer[DmaInfo]

  // Instantiate bridge widgets.
  wrapper.bridgeModuleMap.map({ case (bridgeAnno, bridgeMod) =>
    val widgetChannelPrefix = s"${bridgeAnno.target.ref}"
    bridgeMod.module.reset := reset.toBool || simReset
    bridgeMod match {
      case peekPoke: PeekPokeBridgeModule =>
        peekPoke.module.io.step <> master.module.io.step
        master.module.io.done := peekPoke.module.io.idle
      case _ =>
    }
    bridgeMod.module.hPort.connectChannels2Port(bridgeAnno, simIo)

    bridgeMod.module match {
      case widget: HasDMA => dmaInfoBuffer += DmaInfo(bridgeMod.getWName, widget.dma, widget.dmaSize)
      case _ => Nil
    }
  })

  // Host Memory Channels
  // Masters = Target memory channels + loadMemWidget
  loadMem.module.reset := reset.toBool || simReset

  // Sort the list of DMA ports by address region size, largest to smallest
  val dmaInfoSorted = dmaInfoBuffer.sortBy(_.size).reverse.toSeq
  // Build up the address map using the sorted list,
  // auto-assigning base addresses as we go.
  val dmaAddrMap = dmaInfoSorted.foldLeft((BigInt(0), List.empty[AddrMapEntry])) {
    case ((startAddr, addrMap), DmaInfo(widgetName, _, reqSize)) =>
      // Round up the size to the nearest power of 2
      val regionSize = 1 << log2Ceil(reqSize)
      val region = MemRange(startAddr, regionSize, MemAttr(AddrMapProt.RW))

      (startAddr + regionSize, AddrMapEntry(widgetName, region) :: addrMap)
  }._2.reverse
  val dmaPorts = dmaInfoSorted.map(_.port)

  if (dmaPorts.isEmpty) {
    val dmaParams = p.alterPartial({ case NastiKey => p(DMANastiKey) })
    val error = Module(new NastiErrorSlave()(dmaParams))
    error.io <> io.dma
  } else if (dmaPorts.size == 1) {
    dmaPorts(0) <> io.dma
  } else {
    val dmaParams = p.alterPartial({ case NastiKey => p(DMANastiKey) })
    val router = Module(new NastiRecursiveInterconnect(
      1, new AddrMap(dmaAddrMap))(dmaParams))
    router.io.masters.head <> NastiQueue(io.dma)(dmaParams)
    dmaPorts.zip(router.io.slaves).foreach { case (dma, slave) => dma <> NastiQueue(slave)(dmaParams) }
  }

  wrapper.genCtrlIO(io.ctrl, p(FpgaMMIOSize))

  val addrConsts = dmaAddrMap.map {
    case AddrMapEntry(name, MemRange(addr, _, _)) =>
      (s"${name.toUpperCase}_DMA_ADDR" -> addr.longValue)
  }

  val headerConsts = addrConsts ++ List[(String, Long)](
    "CTRL_ID_BITS"   -> io.ctrl.nastiXIdBits,
    "CTRL_ADDR_BITS" -> io.ctrl.nastiXAddrBits,
    "CTRL_DATA_BITS" -> io.ctrl.nastiXDataBits,
    "CTRL_STRB_BITS" -> io.ctrl.nastiWStrobeBits,
    "MMIO_WIDTH"     -> io.ctrl.nastiWStrobeBits,
    // These specify channel widths; used mostly in the test harnesses
    "MEM_ADDR_BITS"  -> wrapper.hostMemoryBundleParams.addrBits,
    "MEM_DATA_BITS"  -> wrapper.hostMemoryBundleParams.dataBits,
    "MEM_ID_BITS"    -> wrapper.hostMemoryBundleParams.idBits,
    "MEM_STRB_BITS"  -> wrapper.hostMemoryBundleParams.dataBits / 8,
    "MEM_WIDTH"  -> wrapper.hostMemoryBundleParams.dataBits / 8,
    // These are fixed by the AXI4 standard, only used in SW DRAM model
    "MEM_SIZE_BITS"  -> AXI4Parameters.sizeBits,
    "MEM_LEN_BITS"   -> AXI4Parameters.lenBits,
    "MEM_RESP_BITS"  -> AXI4Parameters.respBits,
    // Address width of the aggregated host-DRAM space
    "DMA_ID_BITS"    -> io.dma.nastiXIdBits,
    "DMA_ADDR_BITS"  -> io.dma.nastiXAddrBits,
    "DMA_DATA_BITS"  -> io.dma.nastiXDataBits,
    "DMA_STRB_BITS"  -> io.dma.nastiWStrobeBits,
    "DMA_WIDTH"      -> p(DMANastiKey).dataBits / 8,
    "DMA_SIZE"       -> log2Ceil(p(DMANastiKey).dataBits / 8)
  )
  def genHeader(sb: StringBuilder)(implicit p: Parameters) = wrapper.genHeader(sb)(p(ChannelWidth))

}
