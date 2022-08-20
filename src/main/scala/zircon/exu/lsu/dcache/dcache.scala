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
package zircon.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.utils._
import zircon.mmu._
import difftest._

case class DCacheParams (
                          nSets: Int = 64,
                          nWays: Int = 2,
                          nTLBSets: Int = 64,
                          nTLBWays: Int = 2,
                          blockBytes: Int = 64,
                          fetchBytes: Int = 64,
                          nBeats: Int = 4
                        ) {
  def fetchBits: Int = fetchBytes * 8
  def blockBits: Int = blockBytes * 8
}

class DCacheReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr      = UInt(vaddrBits.W)
  val dtlb_fire = Bool()
  val miss      = Bool()
  val ppn       = UInt(ppnBits.W)
}

class DCacheResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss = Bool()
  val set_idx = UInt(log2Ceil(dcacheParams.nSets).W)
  val way_idx = UInt(log2Ceil(dcacheParams.nWays).W)
  val data = UInt(dcacheParams.fetchBits.W)
}


class DCacheMem(implicit p: Parameters) extends BaseZirconBundle {
  val req   = Decoupled(new MemReq(dcacheParams.blockBits))
  val resp  = Flipped(Decoupled(new MemResp(dcacheParams.blockBits)))
}

class DCacheSFenceReq(implicit p: Parameters) extends BaseZirconBundle {
  val invalid_all = Bool()
}

class DCacheWritePort(implicit p: Parameters) extends BaseZirconBundle {
  val miss    = Bool()
  val set_idx = UInt(log2Ceil(dcacheParams.nSets).W)
  val way_idx = UInt(log2Ceil(dcacheParams.nWays).W)
  val data    = UInt(dcacheParams.blockBits.W)
}

class DCacheModule(implicit p: Parameters) extends BaseZirconModule with MemoryOpConstants {
  val io = IO(new Bundle() {
    //
    val kill = Input(Bool())
    val stall = Input(Bool())
    val sfence = Flipped(Valid(new DCacheSFenceReq))


    //  DCache Require
    val req = Flipped(Valid(new DCacheReq))
    val resp = Output(new DCacheResp)

    //  Memory
    val mem = new DCacheMem
    val write = Flipped(Valid(new DCacheWritePort))

    //  Invalidate
    val invalidate = Input(Vec(dcacheParams.nTLBWays, new DTLBInvalidate))
  })

  def lgSets: Int = log2Ceil(dcacheParams.nSets)
  def lgWays: Int = log2Ceil(dcacheParams.nWays)
  def offBits: Int = log2Ceil(dcacheParams.fetchBytes)
  def tagBits: Int = 1 + ppnBits
  def bankBits: Int = dcacheParams.fetchBits
  def bankIdxBits: Int = log2Ceil(dcacheParams.nBeats)
  def compute_idx(addr: UInt) = addr(offBits + lgSets + bankIdxBits - 1, offBits + bankIdxBits)
  def compute_bank(addr: UInt) = addr(offBits + bankIdxBits - 1, offBits)
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(dcacheParams.nWays) - 1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.asUInt.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(dcacheParams.nWays-1,1)
  }

  def get_replace_way(state: UInt) = {
    val shift_state = state << 1
    var idx = 1.U(1.W)

    for (i <- log2Up(dcacheParams.nWays) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(dcacheParams.nWays)-1, 0) < dcacheParams.nWays.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(dcacheParams.nWays)-1,0)
  }

  //
  class DCacheTagBundle extends Bundle {
    val valid = Bool()
    val ppn   = UInt(ppnBits.W)
  }

  val tag_array   = Mem(dcacheParams.nSets, Vec(dcacheParams.nWays, UInt(tagBits.W)))
  val data_array  = SyncReadMem(dcacheParams.nSets, Vec(dcacheParams.nWays, UInt(dcacheParams.blockBits.W)))
  val plru_array  = Mem(dcacheParams.nSets, UInt((dcacheParams.nWays-1).W))

  //  Require
  val do_req = io.req.valid
  val req_idx = compute_idx(io.req.bits.addr)
  val req_addr = RegEnable(io.req.bits.addr, io.req.valid)

  //  Forward
  val s2_req_idx  = compute_idx(req_addr)
  val refill_idx  = s2_req_idx
  val refill_tag  = Wire(new DCacheTagBundle)
  val refill_data = io.mem.resp.bits.data

  refill_tag.valid := true.B
  refill_tag.ppn   := io.req.bits.ppn


  val do_forward = RegNext(req_idx === s2_req_idx && io.mem.resp.fire)

  //
  val read_tags = RegEnable(tag_array(req_idx), do_req).map(_.asTypeOf(new DCacheTagBundle))
  val read_datas = data_array.read(req_idx, do_req)
  val hit_vec = VecInit(read_tags map { tag =>
    tag.valid && (tag.ppn === io.req.bits.ppn)
  })

  val hold_ena = RegNext(do_req) | io.mem.resp.fire
  val hit_data = HoldUnless(Mux(io.mem.resp.fire, io.mem.resp.bits.data, 
                              Mux(do_forward, RegEnable(refill_data, do_req), Mux1H(hit_vec, read_datas))), hold_ena)
  io.resp.miss := HoldUnless((!hit_vec.reduce(_|_) & !do_forward) & !io.mem.resp.fire, hold_ena)
  io.resp.data := hit_data
  io.resp.set_idx := RegEnable(req_idx, do_req)
  io.resp.way_idx := RegEnable(OHToUInt(hit_vec), do_req)

  //  FSM
  val s_ready::s_request::s_wait::s_wait_invalidate::Nil = Enum(4)
  val state = RegInit(s_ready)

  when ((RegNext(do_req) || io.req.bits.dtlb_fire) && io.resp.miss && !io.req.bits.miss) {
    state := s_request
  }
  when (state === s_request) {
    when (io.sfence.valid) { state := s_wait_invalidate }
    when (io.mem.req.ready) { state := Mux(io.sfence.valid, s_wait_invalidate, s_wait) }
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
  io.mem.req.bits.data  := io.write.bits.data
  io.mem.resp.ready     := !io.stall

  //  Refill
  val refill_sel_tags = tag_array(refill_idx).map(_.asTypeOf(new DCacheTagBundle))
  val has_invalid_way = !refill_sel_tags.map(_.valid).reduce(_&_)
  val invalid_way = OHToUInt(selectFirst(Reverse(Cat(refill_sel_tags.map(!_.valid)))))
  val refill_way = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(refill_idx)))

  //  Refill Valid when
  //  1.  PTW Response Valid.
  //  2.  Has No Exception.
  //  3.  No Invalidation.
  when (io.mem.resp.fire && !io.mem.resp.bits.cause.orR && !io.sfence.valid) {
    printf("[INFO] DCache Refill\n")
    printf("[INFO]    Address: 0x%x, Way: %d, Set: %d\n", req_addr, refill_way, refill_idx)
    printf("[INFO]    Data: 0x%x\n", refill_data)
    for (w <- 0 until dcacheParams.nWays) {
      when (w.U === refill_way) {
        tag_array(refill_idx)(w) := refill_tag.asUInt
      }
    }
    data_array.write(refill_idx, VecInit(Seq.fill(dcacheParams.nWays) {refill_data}), UIntToOH(refill_way).asBools)
  }

  val write_set_idx = io.write.bits.set_idx
  val write_way_idx = io.write.bits.way_idx

  when (io.write.valid) {
    val way_idx = Mux(io.write.bits.miss, refill_way, io.write.bits.way_idx)
    data_array.write(write_set_idx, VecInit(Seq.fill(dcacheParams.nWays){io.write.bits.data}), UIntToOH(way_idx).asBools)
  }

  //  Invalidate Valid when
  //  1.  Invalidate all.
  //  2.  Tag HIT.
  val do_invalid = state === s_wait_invalidate
  val invalid_all = io.sfence.bits.invalid_all
  for (s <- 0 until dcacheParams.nSets) {
    for (w <- 0 until dcacheParams.nWays) {
      when (do_invalid && (invalid_all ||
        io.invalidate.map(i => (i.valid && (i.ppn === tag_array(s)(w).asTypeOf(new DCacheTagBundle).ppn))).reduce(_|_))) {
        tag_array(s)(w).asTypeOf(new DCacheTagBundle).valid := false.B
      }
    }
  }

  //  Update Replacement Valid when
  //  1.  DCache Require Valid and DCache HIT.
  //  2.  DCache Refill Valid.
  when ((RegNext(do_req) && !io.resp.miss && !io.sfence.valid)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), OHToUInt(hit_vec))
  }
  when (io.mem.resp.fire) {
    plru_array(refill_idx) := get_next_state(plru_array(refill_idx), refill_way)
  }
  when (io.write.valid) {
    plru_array(write_set_idx) := get_next_state(plru_array(write_set_idx), write_way_idx)
  }

  if (env.EnableDifftest) {
    val difftest = Module(new DifftestRefillEvent) 
    difftest.io.clock   := clock 
    difftest.io.coreid  := 0.U
    difftest.io.valid   := io.mem.resp.fire && !io.mem.resp.bits.cause.orR && !io.sfence.valid
    difftest.io.addr    := req_addr 
    difftest.io.data    := (0 until 8).map(i => refill_data(64 * (i + 1) - 1, 64 * i))
    difftest.io.cacheid := 1.U  //  DCACHE
  }
  //  End
}

//
//  DCache Top
class DCacheStageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class DCacheStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val dtlb_miss   = Bool()
  val dtlb_data   = UInt(ppnBits.W)
  val dcache_miss = Bool()
  val dcache_data = UInt(dcacheParams.blockBits.W)
  val way_idx     = UInt(log2Ceil(dcacheParams.nWays).W)
  val set_idx     = UInt(log2Ceil(dcacheParams.nSets).W)
  val cause       = UInt(eLen.W)
}

class DCacheIO(implicit p: Parameters) extends BaseZirconBundle {
  //  Hart ID
  val hart_id = Input(UInt(64.W))

  val kill = Input(Bool())
  val stall = Input(Bool())
  val sfence = Flipped(Decoupled(new SFenceReq))
  val ptbr = Input(new PTBR)
  val prv = Input(UInt(2.W))

  //  DCache Require
  val req = Flipped(Valid(new DCacheStageReq))
  val resp = Valid(new DCacheStageResp)

  //  Memory
  val ptw = new DTLBPTW
  val mem = new DCacheMem

  //  Performance
  val dcache_access = Output(Bool())
  val dcache_miss = Output(Bool())
  val dtlb_access = Output(Bool())
  val dtlb_miss = Output(Bool())

  //  Write Port
  val write = Flipped(Valid(new DCacheWritePort))

  //  Busy
  val busy = Output(Bool())
}

class DCache(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new DCacheIO())

  val dtlb = Module(new DTLB(dcacheParams.nTLBSets, dcacheParams.nTLBWays))
  val dcache = Module(new DCacheModule())

  dtlb.io.kill    := io.kill
  dcache.io.kill  := io.kill
  dtlb.io.stall   := io.stall
  dcache.io.stall := io.stall
  dtlb.io.sfence  <> io.sfence
  dtlb.io.ptbr    := io.ptbr
  dtlb.io.prv     := io.prv

  io.ptw <> dtlb.io.ptw
  io.mem <> dcache.io.mem

  //
  val s1_vld = Wire(Bool())
  val s1_addr = io.req.bits.addr
  val s2_vld = Reg(Bool())
  val s2_addr = Reg(UInt(vaddrBits.W))


  //  FSM
  //  State description:
  //  s_ready           : READY
  //  s_itlb_req        : DTLB Require.
  //  s_itlb_wait       : Wait for ITLB Response.
  //  s_icache_req      : DCache Require.
  //  s_icache_wait     : Wait for ICache Response.
  //  s_wait_invalidate : Wiat for invalidate.
  val s_ready::s_dtlb_req::s_dtlb_wait::s_dcache_req::s_dcache_wait::s_wait_invalidate::Nil = Enum(6)
  val state_reg = RegInit(s_ready)
  val state = WireInit(state_reg)

  state := state_reg
  when (RegNext(io.req.valid) && state_reg === s_ready) {
    when (dcache.io.resp.miss) { state := s_dcache_req }
    when (dtlb.io.resp.miss) { state := s_dtlb_req }
  }
  when (state_reg === s_dcache_req) {
    when (io.sfence.valid) { state := s_ready }
    when (io.mem.req.ready) { state := Mux(io.sfence.valid, s_wait_invalidate, s_dcache_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state_reg === s_dtlb_req) {
    when (io.sfence.valid) { state := s_ready }
    when (io.ptw.req.ready) { state := Mux(io.sfence.valid, s_wait_invalidate, s_dtlb_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state_reg === s_dcache_wait) {
    when (io.mem.resp.fire) { state := s_ready }
  }
  when (state_reg === s_dtlb_wait) {
    when (io.ptw.resp.fire) {
      state := Mux(dcache.io.resp.miss && !io.ptw.resp.bits.cause.orR, s_dcache_req, s_ready)
    }
  }
  when (state_reg === s_wait_invalidate) {
    when (io.sfence.fire) { state := s_ready }
  }
  //  Update
  state_reg := state

  io.busy := state =/= s_ready
  val state_is_ready = state === s_ready

  //  Write
  dcache.io.write := io.write

  s1_vld := state_is_ready && io.req.valid && !io.stall
  when (io.kill || io.sfence.valid) {
    s2_vld := false.B
  } .elsewhen (!(io.stall || io.busy)) {
    s2_vld := s1_vld
    s2_addr := s1_addr
  }

  //  DCache Bypass
  val dtlb_cause = Wire(UInt(eLen.W))
  val dcache_cause = Wire(UInt(eLen.W))

  //  Set Default
  dtlb_cause := 0.U
  when (io.ptw.resp.fire) {
    dtlb_cause := io.ptw.resp.bits.cause
  }

  dcache_cause := 0.U
  when (io.mem.resp.fire) {
    dcache_cause := io.mem.resp.bits.cause
  }

  //  DTLB Require
  //  DTLB Require Valid when
  //  1. Current state is READ
  //  2. Send Tag Read Request to DCache.
  dtlb.io.req.valid     := s1_vld
  dtlb.io.req.bits.addr := s1_addr
  io.dtlb_access        := RegNext(s1_vld)
  io.dtlb_miss          := RegNext(io.dtlb_access & dtlb.io.resp.miss)

  //  DCache Require
  //  DCache Require Valid when
  //  1. Current State is READ.
  dcache.io.req.valid     := s1_vld
  dcache.io.req.bits.addr := s1_addr
  dcache.io.req.bits.ppn  := Mux(dtlb.io.resp.miss, io.ptw.resp.bits.pte.ppn, dtlb.io.resp.ppn)
  io.dcache_access        := io.dtlb_access
  io.dcache_miss          := RegNext((io.dcache_access && !dtlb.io.resp.miss && dcache.io.resp.miss) ||
                                    (io.ptw.resp.fire && dcache.io.resp.miss))

  //  DCache invalidate
  //  DCache Invalidate Valid when
  //  1. Do Line Invalid and Clear Valid Bit of Tag.
  dcache.io.sfence.valid := io.sfence.valid
  dcache.io.sfence.bits.invalid_all := !io.sfence.bits.asid_vld && !io.sfence.bits.vpn_vld
  dcache.io.invalidate := dtlb.io.invalidate


  //  Response
  io.resp.valid            := s2_vld 
  io.resp.bits.dtlb_miss   := dtlb.io.resp.miss
  io.resp.bits.dtlb_data   := Mux(dtlb.io.resp.miss, io.ptw.resp.bits.pte.ppn, dtlb.io.resp.ppn)
  io.resp.bits.dcache_miss := dcache.io.resp.miss
  io.resp.bits.dcache_data := Mux(dcache.io.resp.miss, io.mem.resp.bits.data, dcache.io.resp.data)
  io.resp.bits.cause       := dtlb_cause | dcache_cause
  io.resp.bits.way_idx     := dcache.io.resp.way_idx
  io.resp.bits.set_idx     := dcache.io.resp.set_idx

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