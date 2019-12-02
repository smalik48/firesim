// See LICENSE for license details.

package midas.widgets


import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{DecoupledHelper}

trait RequiresHostDRAM {
  self: Widget =>
  def hostDRAMMasterNode: AXI4OutwardNode
  def bytesOfDRAMRequired: BigInt
}
