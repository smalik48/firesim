// See LICENSE.SiFive for license details.

package midas.widgets

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import scala.math.{min,max}

class AXI4AddressOffset(offset: BigInt, capacity: BigInt)(implicit p: Parameters) extends LazyModule {
  val bridgeAddressSet = AddressSet(0, capacity - 1)
  val node = AXI4AdapterNode(
    // TODO: It's only safe to do this if all slaves are homogenous. Assert that.
    slaveFn  = { sp => val s = sp.copy(slaves = Seq(sp.slaves.head.copy(address = Seq(bridgeAddressSet))))
    println(s)
    s},
    masterFn = { p => p })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
      out.aw.bits.addr := in.aw.bits.addr + offset.U
      out.ar.bits.addr := in.ar.bits.addr + offset.U
    }
  }
}

object AXI4AddressOffset
{
  def apply(offset: BigInt, capacity: BigInt)(implicit p: Parameters): AXI4Node = {
    val axi4AddrOffset = LazyModule(new AXI4AddressOffset(offset, capacity))
    axi4AddrOffset.node
  }
}
