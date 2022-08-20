/*
 * Copyright (c) 2022 Lyn
 * Skew is licensed under Mulan PubL v2.
 * You can use this software according to the terms and conditions of the Mulan PubL v2.
 * You may obtain a copy of Mulan PubL v2 at:
 *         http://license.coscl.org.cn/MulanPubL-2.0
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PubL v2 for more details.
 */
package zircon.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import zircon.common._
import zircon.utils._

class AXI4Devices(implicit p: Parameters) extends BaseZirconModule {
  import PLICConstants._

  val io = IO(new Bundle() {
    val node = AXI4SlaveBundle(mmapBusParams)

    val wdogcmp = Vec(PLICConstants.nWdogs, Flipped(new GatewaysIO))
    val rtccmp  = Vec(PLICConstants.nRtcs, Flipped(new GatewaysIO))
    val uart    = Vec(PLICConstants.nUarts, Flipped(new GatewaysIO))
    val qspi    = Vec(PLICConstants.nQspis, Flipped(new GatewaysIO))
    val gpio    = Vec(PLICConstants.nGpios, Flipped(new GatewaysIO))
    val pwm     = Vec(PLICConstants.nPwms, Flipped(new GatewaysIO))

    val rtc_tick = Input(Bool())
    val timer_int = Output(Bool())
    val sft_int = Output(Bool())

    val harts = Output(Vec(nHarts, Bool()))
  })

  val io_in = io.node

  def AddressDecoder(addr: UInt): (Bool, Bool) = {
    val plic_offset = addr - PLICConstants.plicBase.U
    val plic = (plic_offset >= 0.U && plic_offset <= 0x3fffffc.U)
    val valid_clint_regs = Seq(
      ClintConstants.msip.U,
      ClintConstants.timecmp_lo.U,
      ClintConstants.timecmp_hi.U,
      ClintConstants.time_lo.U,
      ClintConstants.time_hi.U
    )
    val clint = isOneOf(addr,  valid_clint_regs)
    (plic, clint)
  }

  val plic = Module(new AXI4Plic())
  plic.io.wdogcmp <> io.wdogcmp
  plic.io.rtccmp  <> io.rtccmp
  plic.io.uart    <> io.uart
  plic.io.qspi    <> io.qspi
  plic.io.gpio    <> io.gpio
  plic.io.pwm     <> io.pwm
  io.harts := plic.io.harts

  val clint = Module(new AXI4Clint())
  clint.io.rtc_tick := io.rtc_tick
  io.timer_int := clint.io.timer_int
  io.sft_int := clint.io.sft_int

  val plic_io_in  = plic.io.node
  val clint_io_in  = clint.io.node

  //  Write Channel
  val (w_plic, w_clint) = AddressDecoder(io_in.aw.bits.addr)
  val w_plic_vld = w_plic || RegEnable(w_plic, io_in.aw.fire)
  val w_clint_vld = w_clint || RegEnable(w_clint, io_in.aw.fire)

  io_in.aw.ready      := Mux(w_plic_vld, plic_io_in.aw.ready, clint_io_in.aw.ready)
  plic_io_in.aw.bits  := io_in.aw.bits
  plic_io_in.aw.valid := w_plic_vld && io_in.aw.valid
  clint_io_in.aw.bits := io_in.aw.bits
  clint_io_in.aw.valid:= w_clint_vld && io_in.aw.valid

  io_in.w.ready       := Mux(w_plic_vld, plic_io_in.w.ready, clint_io_in.w.ready)
  plic_io_in.w.bits   := io_in.w.bits
  plic_io_in.w.valid  := w_plic_vld && io_in.w.valid
  clint_io_in.w.bits  := io_in.w.bits
  clint_io_in.w.valid := w_clint_vld && io_in.w.valid

  io_in.b.valid       := Mux(w_plic_vld, plic_io_in.b.valid, clint_io_in.b.valid)
  io_in.b.bits        := Mux(w_plic_vld, plic_io_in.b.bits, clint_io_in.b.bits)
  plic_io_in.b.ready  := w_plic_vld && io_in.b.ready
  clint_io_in.b.ready := w_clint_vld && io_in.b.ready

  //  Read Channel
  val (r_plic, r_clint) = AddressDecoder(io_in.ar.bits.addr)
  val r_plic_vld = r_plic || RegEnable(r_plic, io_in.ar.fire)
  val r_clint_vld = r_clint || RegEnable(r_clint, io_in.ar.fire)

  io_in.ar.ready      := Mux(r_plic_vld, plic_io_in.ar.ready, clint_io_in.ar.ready)
  plic_io_in.ar.bits  := io_in.ar.bits
  plic_io_in.ar.valid := r_plic_vld && io_in.ar.valid
  clint_io_in.ar.bits := io_in.ar.bits
  clint_io_in.ar.valid:= r_clint_vld && io_in.ar.valid

  io_in.r.valid       := Mux(r_plic_vld, plic_io_in.r.valid, clint_io_in.r.valid)
  io_in.r.bits        := Mux(r_plic_vld, plic_io_in.r.bits, clint_io_in.r.bits)
  plic_io_in.r.ready  := r_plic_vld & io.node.r.ready
  clint_io_in.r.ready := r_clint_vld && io.node.r.ready

  //  End
}