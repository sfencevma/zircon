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
package zircon.exu.lsu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.exu.lsu.dcache._
import zircon.issue._
import zircon.mmu._
import zircon.util._

class LSUPTWIO(implicit p: Parameters) extends BaseZirconBundle {
  val dtlb_ptw = new DTLBPTW
  val dcache_ptw = new DCachePTW
}

class LSUDCacheSyncReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(paddrBits.W)
  val data = UInt(dcacheParams.blockBits.W)
}

class LSUDCacheSyncResp(implicit p: Parameters) extends BaseZirconBundle {
  val cause = UInt(eLen.W)
}

class LSUDCacheSyncIO(implicit p: Parameters) extends BaseZirconBundle {
  val req = Decoupled(new LSUDCacheSyncReq)
  val resp = Flipped(Valid(new LSUDCacheSyncResp))
}

class LSUIO(implicit p: Parameters) extends BaseZirconBundle {
  //  Flush
  val flush = Input(Bool())

  //  SFence
  val sfence = Flipped(Decoupled(new SFenceReq))

  //  Require
  val req = Input(new IssueResp)
  val resp = Output(new RobExecReq)

  //  Write Back
  val writeback_req = Output(new IssueWriteBack)
  val replay = Output(new IssueReplay)

  //  Write DCache
  val store_exec_req = Flipped(new RobStoreExecIO)
  val dcache_sync = new LSUDCacheSyncIO

  //  MMU
  val ptw = new LSUPTWIO

  val ldq_head = Input(UInt(ldqIdBits.W))
  val ldq_tail = Input(UInt(ldqIdBits.W))
  val stq_head = Input(UInt(stqIdBits.W))
  val stq_tail = Input(UInt(stqIdBits.W))

  //  Forward Stall
  val fstall = Output(Bool())
}

class LSU(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new LSUIO)

  val pipeline_ena = Wire(Bool())

  //  -----------------------------------------------------------
  //  Generate PC
  def offBits: Int = log2Ceil(dcacheParams.blockBytes)

  val dw = io.req.uop.dw
  val lsu_addr = io.req.uop.lrs1 + io.req.uop.lrs2
  val s1_addr = Mux(io.req.secondary,  lsu_addr + 64.U, lsu_addr)

  val s1_cr_page = (lsu_addr(pgIdxBits-1, 0) === 0xfff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
    (lsu_addr(pgIdxBits-1, 0) === 0xffe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
    (lsu_addr(pgIdxBits-1, 0) === 0xffd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
    (dw === DW_64 && isOneOf(lsu_addr(pgIdxBits-1,0), (0 until 4).map(i => (0xffc - i).U)))
  val s1_cr_line = (lsu_addr(offBits-1, 0) === 0xff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
    (lsu_addr(offBits-1, 0) === 0xfe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
    (lsu_addr(offBits-1, 0) === 0xfd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
    (dw === DW_64 && isOneOf(lsu_addr(offBits-1,0), (0 until 4).map(i => (0xfc - i).U)))

  val s1_cross = s1_cr_page || s1_cr_page

  //  -----------------------------------------------------------
  //  Read DCache
  val s2_req = RegEnable(io.req, pipeline_ena)
  val s2_addr = RegEnable(s1_addr, pipeline_ena)
  val s2_cr_line = RegEnable(s1_cross, pipeline_ena)

  val ldq = Module(new LDQ)
  val stq = Module(new STQ)
  val dtlb = Module(new DTLB(dcacheParams.nSets, dcacheParams.nWays))
  val dcache = Module(new DCache)

  ldq.io.head := io.ldq_head
  ldq.io.tail := io.ldq_tail
  stq.io.head := io.stq_head
  stq.io.tail := io.stq_tail

  class MSHREntry extends Bundle {
    val is_phy      = Bool()
    val addr        = UInt(paddrBits.W)
    val is_ld       = Bool()
    val ld_id       = UInt(ldqIdBits.W)
    val is_st       = Bool()
    val st_id       = UInt(stqIdBits.W)
    val secondary   = Bool()
  }

  val mshr = Module(new Queue(new MSHREntry, nMSHRs, hasFlush = true))
  mshr.io.flush.get := io.flush

  //  Step 1: Read DTLB & DCache & STQ (if need)
  dtlb.io.req.valid := s2_req.valid
  dtlb.io.req.bits.addr := s2_addr
  dcache.io.req.valid := s2_req.valid & s2_req.uop.is_ld
  dcache.io.req.bits.addr := s2_addr
  dcache.io.req.bits.ppn := dtlb.io.resp.ppn
  stq.io.snoop.req.valid := s2_req.valid & s2_req.uop.is_ld
  stq.io.snoop.req.bits.addr := s2_addr
  stq.io.snoop.req.bits.dw := s2_req.uop.dw

  //  Step 2: Check whether miss
  val dtlb_miss = dtlb.io.resp.miss
  val dcache_miss = dcache.io.resp.miss
  val s2_data = Mux(s2_req.uop.is_ld,
    Mux(stq.io.snoop.resp.valid, stq.io.snoop.resp.bits.data, dcache.io.resp.data), zeroExtend(dtlb.io.resp.ppn, xLen))

  //  Step 3: MSHR
  //  1.  Load. 1) If Snoop hit, go on. 2) DTLB or DCache miss, to MSHR.
  //  2.  Store. 1) If DTLB miss, to MSHR.
  val load_miss = s2_req.uop.is_ld & (!stq.io.snoop.resp.valid || dtlb_miss || dcache_miss)
  val store_miss = s2_req.uop.is_st & dtlb_miss
  val lsu_miss = load_miss || store_miss

  //  New MSHR Require
  val new_req = Wire(new MSHREntry)
  new_req.is_phy := !dtlb_miss
  new_req.addr  := Mux(dtlb_miss, s2_addr, Cat(dtlb.io.resp.ppn, s2_addr(pgIdxBits-1,0)))
  new_req.is_ld := s2_req.uop.is_ld
  new_req.ld_id := s2_req.uop.ld_id
  new_req.is_st := s2_req.uop.is_st
  new_req.st_id := s2_req.uop.st_id
  new_req.secondary := s2_req.secondary

  mshr.io.enq.valid := s2_req.valid & lsu_miss
  mshr.io.enq.bits := new_req
  pipeline_ena := mshr.io.count < nMSHRs.U

  //  Step 4: Write to STQ (if need)
  stq.io.req.valid          := s2_req.valid && s2_req.uop.is_st && !dtlb_miss
  stq.io.req.bits.rob_id    := s2_req.uop.rob_id
  stq.io.req.bits.st_id     := s2_req.uop.st_id
  stq.io.req.bits.secondary := s2_req.secondary
  stq.io.req.bits.is_amo    := s2_req.uop.is_amo
  stq.io.req.bits.addr      := s2_addr
  stq.io.req.bits.data      := s2_req.uop.lrs2
  stq.io.req.bits.dw        := s2_req.uop.dw
  stq.io.req.bits.cr_line   := s2_cr_line
  stq.io.req.bits.ppn       := s2_data

  //  Step 5: MSHR to MMU
  //  io.ptw.req.valid := mshr.io.deq.valid
  val mshr_req = mshr.io.deq.bits
  io.ptw.dtlb_ptw.req.valid       := mshr.io.deq.valid && !mshr_req.is_phy
  io.ptw.dtlb_ptw.req.bits.addr   := mshr_req.addr
  io.ptw.dcache_ptw.req.valid     := mshr.io.deq.valid && mshr_req.is_phy
  io.ptw.dcache_ptw.req.bits.addr := mshr_req.addr
  mshr.io.deq.ready               := Mux(mshr_req.is_phy, io.ptw.dtlb_ptw.req.ready, io.ptw.dcache_ptw.req.ready)

  dtlb.io.ptw.resp := io.ptw.dtlb_ptw.resp
  dcache.io.ptw.resp := io.ptw.dcache_ptw.resp

  //  Step 6: MSHR to STQ, LDQ
  ldq.io.mshr_req.valid := io.ptw.dcache_ptw.resp.valid
  ldq.io.mshr_req.bits.data := DontCare
  ldq.io.mshr_req.bits.cause := io.ptw.dcache_ptw.resp.bits.cause

  stq.io.mshr_req.valid := io.ptw.dtlb_ptw.resp.valid
  stq.io.mshr_req.bits.secondary := DontCare
  stq.io.mshr_req.bits.cause := io.ptw.dtlb_ptw.resp.bits.cause

  //  -----------------------------------------------------------
  //  Write Back
  val s3_req = RegEnable(s2_req, pipeline_ena)
  val s3_cr_line = RegEnable(s2_cr_line, pipeline_ena)
  val s3_miss = RegEnable(lsu_miss, pipeline_ena)
  val s3_data = RegEnable(s2_data, pipeline_ena)

  io.replay.replay := s3_req.valid & (s3_miss | s3_cr_line)
  io.replay.grant := s3_req.valid & s3_miss
  io.replay.secondary := Mux(!s3_miss & s3_cr_line, true.B, s3_req.secondary)

  io.writeback_req.valid := s3_req.valid & !s3_miss
  io.writeback_req.pntr := s3_req.uop.rob_id
  io.writeback_req.data := s3_data

  //  -----------------------------------------------------------
  //  DCache FSM
  val s_idle::s_tag_req::s_invalid::s_sync_req::s_sync_wait::s_done::Nil = Enum(4)
  val state_reg = RegInit(s_idle)
  val state = WireInit(state_reg)

  state := state_reg
  switch (state_reg) {

  }

  when (io.sfence.valid) {
    state_reg := state
  } .elsewhen (io.flush) {
    state_reg := s_idle
  } .otherwise {
    state_reg := state
  }

  //  ----------------------------------------------------------
  //  Write DCache
}