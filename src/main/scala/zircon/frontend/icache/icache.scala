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
package zircon.frontend

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.utils._
import zircon.exu._
import zircon.mmu._
import difftest._

case class ICacheParams (
                          nSets: Int = 64,
                          nWays: Int = 2,
                          nTLBSets: Int = 64,
                          nTLBWays: Int = 2,
                          blockBytes: Int = 64,
                          fetchBytes: Int = 16,
                          nBeats: Int = 4
                        ) {
  def fetchBits: Int = fetchBytes * 8
  def blockBits: Int = blockBytes * 8
}

class ICacheReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr       = UInt(vaddrBits.W)
  val itlb_fire  = Bool()
  val miss       = Bool()
  val ppn        = UInt(ppnBits.W)
}

class ICacheResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss = Bool()
  val data = UInt(icacheParams.fetchBits.W)
}

class ICacheMem(implicit p: Parameters) extends BaseZirconBundle {
  val req   = Decoupled(new MemReq(icacheParams.blockBits))
  val resp  = Flipped(Decoupled(new MemResp(icacheParams.blockBits)))
}

class ICacheSFenceReq(implicit p: Parameters) extends BaseZirconBundle {
  val invalid_all = Bool()
}

class ICacheModule(implicit p: Parameters) extends BaseZirconModule with MemoryOpConstants {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val stall = Input(Bool())
    val sfence = Flipped(Valid(new ICacheSFenceReq))

    val req = Flipped(Valid(new ICacheReq))
    val resp = Output(new ICacheResp)
    val mem = new ICacheMem

    val invalidate = Input(Vec(icacheParams.nTLBWays, new ITLBInvalidate))
  })

  def lgSets: Int = log2Ceil(icacheParams.nSets)
  def lgWays: Int = log2Ceil(icacheParams.nWays)
  def offBits: Int = log2Ceil(icacheParams.fetchBytes)
  def tagBits: Int = 1 + ppnBits
  def bankBits: Int = icacheParams.fetchBits
  def bankIdxBits: Int = log2Ceil(icacheParams.nBeats)
  def compute_idx(addr: UInt) = addr(offBits + lgSets + bankIdxBits - 1, offBits + bankIdxBits)
  def compute_bank(addr: UInt) = addr(offBits + bankIdxBits - 1, offBits)
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(icacheParams.nWays) - 1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.asUInt.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(icacheParams.nWays-1,1)
  }

  def get_replace_way(state: UInt) = {
    val shift_state = state << 1
    var idx = 1.U(1.W)

    for (i <- log2Up(icacheParams.nWays) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(icacheParams.nWays)-1, 0) < icacheParams.nWays.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(icacheParams.nWays)-1,0)
  }

  //
  class ICacheTagBundle extends Bundle {
    val valid = Bool()
    val ppn   = UInt(ppnBits.W)
  }

  val tag_array   = Mem(icacheParams.nSets, Vec(icacheParams.nWays, UInt(tagBits.W)))
  val data_array  = SyncReadMem(icacheParams.nSets, Vec(icacheParams.nWays, UInt(icacheParams.blockBits.W)))
  val plru_array  = Mem(icacheParams.nSets, UInt((icacheParams.nWays-1).W))

  //  Require
  val do_req = io.req.valid
  val req_idx = compute_idx(io.req.bits.addr)
  val req_addr = RegEnable(io.req.bits.addr, do_req)
  val req_bank = compute_bank(req_addr)

  //  Forward 
  val s2_req_idx  = compute_idx(req_addr)
  val refill_idx  = s2_req_idx
  val refill_tag  = Wire(new ICacheTagBundle)
  val refill_data = io.mem.resp.bits.data

  refill_tag.valid := true.B
  refill_tag.ppn   := io.req.bits.ppn

  val do_forward = RegNext(req_idx === s2_req_idx && io.mem.resp.fire)
  //  
  val read_tags = RegEnable(tag_array(req_idx), do_req).map(_.asTypeOf(new ICacheTagBundle)) 
  val read_datas = data_array.read(req_idx, do_req)
  val hit_vec = read_tags map { tag =>
    tag.valid && (tag.ppn === io.req.bits.ppn)
  }

  val hold_ena = RegNext(do_req) | io.mem.resp.fire
  val hit_data = HoldUnless(Mux(io.mem.resp.fire, io.mem.resp.bits.data, 
                    Mux(do_forward, RegEnable(refill_data, do_req), Mux1H(hit_vec, read_datas))), hold_ena)
  io.resp.miss := HoldUnless((!hit_vec.reduce(_|_) & !do_forward) & !io.mem.resp.fire, hold_ena)
  io.resp.data := MuxCase(0.U,
    Array(
      (req_bank === 0.U) -> hit_data(1 * bankBits - 1,            0),
      (req_bank === 1.U) -> hit_data(2 * bankBits - 1, 1 * bankBits),
      (req_bank === 2.U) -> hit_data(3 * bankBits - 1, 2 * bankBits),
      (req_bank === 3.U) -> hit_data(4 * bankBits - 1, 3 * bankBits)
    )
  )

  //  FSM
  val s_ready::s_request::s_wait::s_wait_invalidate::Nil = Enum(4)
  val state = RegInit(s_ready)

  when ((RegNext(do_req) || io.req.bits.itlb_fire) && io.resp.miss && !io.req.bits.miss) { //  ITLB no miss
    state := s_request
  }
  when (state === s_request) {
    when (io.sfence.valid) { state := s_wait_invalidate }
    when (io.mem.req.fire) { state := Mux(io.sfence.valid, s_wait_invalidate, s_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state === s_wait) {
    state := Mux(io.sfence.valid, s_wait_invalidate, Mux(io.mem.resp.fire, s_ready, state))
  }

  io.mem.req.valid      := state === s_request
  io.mem.req.bits.addr  := req_addr
  io.mem.req.bits.cmd   := M_XRD
  io.mem.req.bits.len   := 4.U
  io.mem.req.bits.size  := memBusParams.SIZE_16B
  io.mem.req.bits.data  := DontCare
  io.mem.resp.ready     := !io.stall
  
  //  Refill
  val refill_sel_tags = tag_array(refill_idx).map(_.asTypeOf(new ICacheTagBundle))
  val has_invalid_way = !refill_sel_tags.map(_.valid).reduce(_&_)
  val invalid_way = OHToUInt(selectFirst(Reverse(Cat(refill_sel_tags.map(!_.valid)))))
  val refill_way = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(refill_idx)))

  //  Refill Valid when
  //  1.  PTW Response Valid.
  //  2.  Has No Exception.
  //  3.  No Invalidation.
  when (io.mem.resp.fire && !io.mem.resp.bits.cause.orR && !io.sfence.valid) {
    printf("[INFO] ICache Refill\n")
    printf("[INFO]    Address: 0x%x, Way: %d, Set: %d\n", req_addr, refill_way, refill_idx)
    printf("[INFO]    Data: 0x%x\n", refill_data)
    //  TODO: Add logger

    for (w <- 0 until icacheParams.nWays) {
      when (w.U === refill_way) {
        tag_array(refill_idx)(w) := refill_tag.asUInt
      }
    }
    data_array.write(refill_idx, VecInit(Seq.fill(icacheParams.nWays) {refill_data}), UIntToOH(refill_way).asBools)
  }

  //  Invalidate Valid when
  //  1.  Invalidate all.
  //  2.  Tag HIT.
  val do_invalid = state === s_wait_invalidate
  val invalid_all = io.sfence.bits.invalid_all
  for (s <- 0 until icacheParams.nSets) {
    for (w <- 0 until icacheParams.nWays) {
      when (do_invalid && (invalid_all ||
        io.invalidate.map(i => (i.valid && (i.ppn === tag_array(s)(w).asTypeOf(new ICacheTagBundle).ppn))).reduce(_|_))) {
        tag_array(s)(w).asTypeOf(new ICacheTagBundle).valid := false.B
      }
    }
  }

  //  Update Replacement Valid when
  //  1.  ICache Require Valid and ICache HIT.
  //  2.  ICache Refill Valid.
  when (((RegNext(do_req) || io.req.bits.itlb_fire) && !io.resp.miss && !io.sfence.valid)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), OHToUInt(hit_vec))
  }
  when (io.mem.resp.fire) {
    plru_array(refill_idx) := get_next_state(plru_array(refill_idx), refill_way)
  }

  if (env.EnableDifftest) {
    val difftest = Module(new DifftestRefillEvent) 
    difftest.io.clock := clock 
    difftest.io.coreid := 0.U
    difftest.io.valid := io.mem.resp.fire && !io.mem.resp.bits.cause.orR && !io.sfence.valid
    difftest.io.addr := req_addr 
    difftest.io.data := (0 until 8).map(i => refill_data(64 * (i + 1) - 1, 64 * i))
    difftest.io.cacheid := 0.U  //  ICACHE
  }
  //  End
}

//
class ICacheStageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class ICacheStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val addr  = UInt(vaddrBits.W)
  val data  = UInt(icacheParams.fetchBits.W)
  val cause = UInt(eLen.W)
}

class ICacheIO(implicit p: Parameters) extends BaseZirconBundle {
  val hart_id = Input(UInt(64.W))

  val kill = Input(Bool())
  val stall = Input(Bool())

  val ptbr = Input(new PTBR)
  val prv = Input(UInt(2.W))
  val sfence = Flipped(Decoupled(new SFenceReq))

  val req = Flipped(Valid(new ICacheStageReq))
  val resp = Valid(new ICacheStageResp)

  val ptw = new ITLBPTW
  val mem = new ICacheMem

  val icache_access = Output(Bool())
  val icache_miss = Output(Bool())
  val itlb_access = Output(Bool())
  val itlb_miss = Output(Bool())

  val busy = Output(Bool())
}

class ICache(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new ICacheIO())

  val itlb = Module(new ITLB(icacheParams.nTLBSets, icacheParams.nTLBWays))
  val icache = Module(new ICacheModule())

  itlb.io.kill    := io.kill
  icache.io.kill  := io.kill
  itlb.io.stall   := io.stall
  icache.io.stall := io.stall
  itlb.io.sfence  <> io.sfence
  itlb.io.ptbr    := io.ptbr
  itlb.io.prv     := io.prv

  io.ptw <> itlb.io.ptw
  io.mem <> icache.io.mem

  //
  val s1_vld = Wire(Bool())
  val s1_addr = io.req.bits.addr
  val s2_vld = Reg(Bool())
  val s2_addr = Reg(UInt(vaddrBits.W))

  //  FSM
  //  State description:
  //  s_ready           : READY
  //  s_itlb_req        : ITLB Require.
  //  s_itlb_wait       : Wait for ITLB Response.
  //  s_icache_req      : ICache Require.
  //  s_icache_wait     : Wait for ICache Response.
  //  s_wait_invalidate : Wiat for invalidate.
  val s_ready::s_itlb_req::s_itlb_wait::s_icache_req::s_icache_wait::s_wait_invalidate::Nil = Enum(6)
  val state_reg = RegInit(s_ready)
  val state = WireInit(state_reg)

  state := state_reg
  when (RegNext(io.req.valid) && state_reg === s_ready) {
    when (icache.io.resp.miss) { state := s_icache_req }
    when (itlb.io.resp.miss) { state := s_itlb_req }
  }
  when (state_reg === s_icache_req) {
    when (io.sfence.valid) { state := s_ready }
    when (io.mem.req.ready) { state := Mux(io.sfence.valid, s_wait_invalidate, s_icache_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state_reg === s_itlb_req) {
    when (io.sfence.valid) { state := s_ready }
    when (io.ptw.req.ready) { state := Mux(io.sfence.valid, s_wait_invalidate, s_itlb_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state_reg === s_icache_wait) {
    when (io.mem.resp.fire) { state := s_ready }
  }
  when (state_reg === s_itlb_wait) {
    when (io.ptw.resp.fire) {
      state := Mux(icache.io.resp.miss & !io.ptw.resp.bits.cause.orR, s_icache_req, s_ready)
    }
  }
  when (state_reg === s_wait_invalidate) {
    when (io.sfence.fire) { state := s_ready }
  }
  //  Update
  state_reg := state

  io.busy := state =/= s_ready
  val state_is_ready = state === s_ready

  //  Pipeline
  s1_vld := state_is_ready && io.req.valid && !io.stall
  when (io.kill || io.sfence.valid) {
    s2_vld := false.B
  } .elsewhen (!(io.stall || io.busy)) {
    s2_vld := s1_vld
    s2_addr := s1_addr
  }

  //  Load Exception
  val itlb_cause = Reg(UInt(eLen.W))
  val icache_cause = Reg(UInt(eLen.W))

  itlb_cause := 0.U
  when (io.ptw.resp.fire) {
    itlb_cause := io.ptw.resp.bits.cause
  }

  icache_cause := 0.U
  when (io.mem.resp.fire) {
    icache_cause := io.mem.resp.bits.cause
  }

  //  ITLB Require
  //  ITLB Require Valid when
  //  1. Current state is READ
  //  2. Send Tag Read Request to ICache.
  itlb.io.req.valid     := s1_vld
  itlb.io.req.bits.addr := s1_addr
  io.itlb_access        := RegNext(s1_vld)
  io.itlb_miss          := RegNext(io.itlb_access & itlb.io.resp.miss)

  //  ICache Require
  //  ICache Require Valid when
  //  1. Current State is READ.
  icache.io.req.valid     := s1_vld
  icache.io.req.bits.addr := s1_addr
  icache.io.req.bits.ppn  := Mux(itlb.io.resp.miss, io.ptw.resp.bits.pte.ppn, itlb.io.resp.ppn)
  icache.io.req.bits.itlb_fire := io.ptw.resp.fire 
  icache.io.req.bits.miss := itlb.io.resp.miss
  io.icache_access        := io.itlb_access
  io.icache_miss          := RegNext((io.icache_access && !itlb.io.resp.miss && icache.io.resp.miss) ||
                                    (io.ptw.resp.fire && icache.io.resp.miss))

  //  ICache invalidate
  //  ICache Invalidate Valid when
  //  1. Do Line Invalid and Clear Valid Bit of Tag.
  icache.io.sfence.valid := io.sfence.valid
  icache.io.sfence.bits.invalid_all := !io.sfence.bits.asid_vld && !io.sfence.bits.vpn_vld
  icache.io.invalidate := itlb.io.invalidate

  //
  val icache_bypass_data = Mux1H(UIntToOH(s2_addr(5, 4)), (0 until 4).map(i => io.mem.resp.bits.data(128 * (i+1)-1, 128 * i)))

  //  ICache to Scan
  val resp_valid = Reg(Bool())
  val resp_addr = Reg(UInt(vaddrBits.W))
  val resp_data = Reg(UInt(icacheParams.fetchBits.W))

  val data_ready = s2_vld && (!itlb.io.resp.miss && !icache.io.resp.miss) 
  when (io.kill || io.sfence.valid) {
    resp_valid := false.B
  } .elsewhen (io.stall) {
    resp_valid := resp_valid
  } .otherwise {
    resp_valid := data_ready 
  }

  when (!io.stall)  {
    resp_addr := s2_addr
    resp_data := icache.io.resp.data
  }

  io.resp.valid       := resp_valid
  io.resp.bits.addr   := resp_addr
  io.resp.bits.data   := resp_data
  io.resp.bits.cause  := itlb_cause | icache_cause

  //  Difftest
  if (env.EnableDifftest) {
    val paddr = RegEnable(io.mem.req.bits.addr, io.mem.req.fire)
    val difftest = Module(new DifftestRefillEvent)
    difftest.io.clock   := clock 
    difftest.io.coreid  := io.hart_id
    difftest.io.cacheid := 0.U
    difftest.io.valid   := io.mem.resp.fire
    difftest.io.addr    := paddr 
    difftest.io.data    := VecInit((0 until 8).map(i => io.mem.resp.bits.data(64 * (i+1)-1, 64*i)))
  }

  //   End
}