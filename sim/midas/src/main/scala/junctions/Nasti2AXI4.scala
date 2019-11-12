/// See LICENSE for license details.

package junctions

import chisel3._
import chisel3.experimental.MultiIOModule

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config.{Parameters}

class AXI42NastiIdentityModule(params: AXI4BundleParameters)(implicit p: Parameters) extends MultiIOModule {
  val axi4 = IO(Flipped(new AXI4Bundle(params)))
  val nasti = IO(new NastiIO()(p alterPartial { case NastiKey => NastiParameters(params) } ))

  import chisel3.core.ExplicitCompileOptions.NotStrict
  nasti <> axi4
  nasti.ar.bits.user := axi4.ar.bits.user.getOrElse(DontCare)
  nasti.aw.bits.user := axi4.aw.bits.user.getOrElse(DontCare)
}

class Nasti2AXI4IdentityModule(params: AXI4BundleParameters)(implicit p: Parameters) extends MultiIOModule {
  val axi4 = IO(new AXI4Bundle(params))
  val nasti = IO(Flipped(new NastiIO()(p alterPartial { case NastiKey => NastiParameters(params) } )))
  import chisel3.core.ExplicitCompileOptions.NotStrict
  axi4 <> nasti
  nasti.r.bits.user := axi4.r.bits.user.getOrElse(DontCare)
  nasti.b.bits.user := axi4.b.bits.user.getOrElse(DontCare)
}

object Nasti2AXI4 {
  def convertFromAXI4Sink(axi4Sink: AXI4Bundle)(implicit p: Parameters): NastiIO = {
    val conv = Module(new Nasti2AXI4IdentityModule(axi4Sink.params))
    axi4Sink <> conv.axi4
    conv.nasti
  }
}
