// See LICENSE for license details.

package midas.widgets

import midas.widgets.CppGenerationUtils._
import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{DecoupledHelper}

trait RequiresHostDRAM {
  self: Widget =>
  def hostDRAMMasterNode: AXI4OutwardNode
  def bytesOfDRAMRequired: BigInt

  def collasceLikeBridges: Boolean = false
  def targetMemoryRegionName: Option[String] = None
}

case class TargetToHostMemoryMapping(regionName: String, hostOffset: BigInt) {
  def serializeToHeader(sb: StringBuilder): Unit = {
    sb.append(genComment(s"Target to host memory mapping for region: ${regionName}"))
    sb.append(genConstStatic(s"${regionName}_offset", UInt64(hostOffset)))
  }
}
