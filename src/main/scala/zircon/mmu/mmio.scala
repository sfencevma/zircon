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
package zircon.mmu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.Causes
import zircon.axi4._
import zircon.common._

class MMIOReq(implicit p: Parameters) extends BaseZirconBundle with MemoryOpConstants {
  val addr = UInt(vaddrBits.W)
  val cmd  = UInt(M_SZ.W)
  val data = UInt(xLen.W)
}

class MMIOResp(implicit p: Parameters) extends BaseZirconBundle {
  val data  = UInt(xLen.W)
  val cause = UInt(eLen.W)
}

class MMIO(implicit p: Parameters) extends BaseZirconModule with MemoryOpConstants {
  val io = IO(new Bundle() {
    val node = AXI4MasterBundle(mmapBusParams)

    val req = Flipped(Decoupled(new MMIOReq))
    val resp = Decoupled(new MMIOResp)
  })

  val io_out = io.node

  //  State description
  //  s_ready       : Ready!
  //  s_write_req   : Write data.
  //  s_write_wait  : Wait for write response.
  //  s_read_req    : Read data.
  //  s_read_wait   : Wait for read response.
  //  s_done        : Transfer success.
  val s_ready::s_write_req::s_write_data::s_write_wait::s_read_req::s_read_wait::s_done::Nil = Enum(7)
  val state = RegInit(s_ready)

  when (io.req.fire) {
    when (io.req.bits.cmd === M_XWR) { state := s_write_req }
    when (io.req.bits.cmd === M_XRD) { state := s_read_req }
  }
  when (state === s_write_req) {
    when (io_out.aw.fire) { state := s_write_data }
  }
  when (state === s_write_data && io_out.w.bits.last) {
    when (io_out.w.fire) { state := s_write_wait }
  }
  when (state === s_write_wait) {
    when (io_out.b.fire) { state := s_done }
  }
  when (state === s_read_req) {
    when (io_out.ar.fire) { state := s_read_wait }
  }
  when (state === s_read_wait) {
    when (io_out.r.fire && io_out.r.bits.last) { state := s_done }
  }
  when (state === s_done) {
    when (io.resp.fire) { state := s_ready }
  }

  //
  //  Write Channel
  io_out.aw.valid      := state === s_write_req
  io_out.aw.bits.id    := 0.U
  io_out.aw.bits.addr  := io.req.bits.addr
  io_out.aw.bits.len   := 1.U
  io_out.aw.bits.size  := io_out.params.SIZE_8B
  io_out.aw.bits.burst := io_out.params.BURST_INCR
  io_out.aw.bits.lock  := 0.U
  io_out.aw.bits.cache := io_out.params.CACHE_NONBUFFERABLE
  io_out.aw.bits.prot  := io_out.params.PROT_INSECURE
  io_out.aw.bits.qos   := 0.U
  io_out.aw.bits.user  := 0.U

  io_out.w.valid      := state === s_write_data
  io_out.w.bits.data  := io.req.bits.data
  io_out.w.bits.strb  := Fill(xLen/8, 1.U)
  io_out.w.bits.last  := 1.U

  io_out.b.ready      := state === s_write_wait

  io.req.ready := state === s_ready

  //  Read Channel
  io_out.ar.valid      := state === s_read_req
  io_out.ar.bits.id    := 0.U
  io_out.ar.bits.addr  := io.req.bits.addr
  io_out.ar.bits.len   := 1.U
  io_out.ar.bits.size  := io_out.params.SIZE_8B
  io_out.ar.bits.burst := io_out.params.BURST_INCR
  io_out.ar.bits.lock  := 0.U
  io_out.ar.bits.cache := io_out.params.CACHE_NONBUFFERABLE
  io_out.ar.bits.prot  := io_out.params.PROT_INSECURE
  io_out.ar.bits.qos   := 0.U
  io_out.ar.bits.user  := 0.U

  io_out.r.ready      := state === s_read_wait

  val cause = Reg(UInt(eLen.W))
  val data = Reg(UInt(xLen.W))
  when (io_out.b.fire) {
    cause := Mux(io_out.b.bits.resp =/= io_out.params.RESP_OKAY, Causes.store_access.U, 0.U)
  } .elsewhen (io_out.r.fire) {
    data := io_out.r.bits.data
    cause := Mux(io_out.r.bits.resp =/= io_out.params.RESP_OKAY, Causes.load_access.U, 0.U)
  }

  //  Response
  io.resp.valid       := state === s_done
  io.resp.bits.data   := data
  io.resp.bits.cause  := cause

  //  End
}