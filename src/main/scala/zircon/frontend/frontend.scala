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
import zircon.exu._
import zircon.mmu._

trait HasFrontendParameters {
  val numBIMSets: Int = 2048
  val numFBTBSets: Int = 128
  val numFetchBufferEntries: Int = 32
}

class FrontendRedirect(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class FrontendPerfEvents(implicit p: Parameters) extends BaseZirconBundle {
  val icache_access = Bool()
  val icache_miss   = Bool()
  val itlb_access   = Bool()
  val itlb_miss     = Bool()
}

class Frontend(implicit p: Parameters) extends BaseZirconModule
  with HasFrontendParameters
{
  val io = IO(new Bundle() {
    val hart_id       = Input(UInt(64.W))
    val reset_vector  = Input(UInt(vaddrBits.W))
    //
    val kill = Flipped(Valid(new FrontendRedirect))
    val stall = Input(Bool())
    val ptbr = Input(new PTBR)
    val prv = Input(UInt(2.W))
    val sfence = Flipped(Decoupled(new SFenceReq))
    val sync = Input(Bool())

    //
    val bpd_upd = Flipped(Decoupled(new PredictorUpdate))
    val resp = Valid(Vec(decodeWidth, new FetchBufferResp))

    val ptw = new ITLBPTW
    val mem = new ICacheMem

    val perf = Output(new FrontendPerfEvents())
  })

  //  *********************************************
  //  Core PC
  //  Boot Address is 0x000000000000, can be changed.
  val reset_vector = io.reset_vector
  val core_pc = RegInit(UInt(vaddrBits.W), reset_vector)

  //  *********************************************
  //  Bimodal table
  val bim = Module(new BIM(numBIMSets))

  //  *********************************************
  //  Fast BTB
  val fbtb = Module(new FastBTB(numFBTBSets))

  //  *********************************************
  //  Stage 2-3
  val icache = Module(new ICache)

  //  *********************************************
  //  Stage 4
  val scan = Module(new ScanStage(decodeWidth))

  //  *********************************************
  //  Stage 5
  val fbuf = Module(new FetchBuffer(decodeWidth, numFetchBufferEntries))

  //  *********************************************
  //  Stage 6

  //
  val s1_vld  = WireInit(false.B)
  val s1_addr = Wire(UInt())
  val s2_vld  = Reg(Bool())
  val s2_addr = Reg(UInt(vaddrBits.W))
  val s3_cnt  = Reg(UInt(2.W))
  val s4_cnt  = Reg(UInt(2.W))

  // **************************************
  //  Stage 1: Generate PC
  //  Description:
  //  s_idle:         The original status, will be reset by global reset signal. In order
  //                  to eliminate half cycle.
  //  s_gen:          Generate PC.
  val s_idle::s_gen::Nil = Enum(2)
  val state = RegInit(s_idle)

  //  Next-Line Predictor Redirect Valid when
  //  1.  Stage 2 Valid.
  //  2.  Fast BTB Hit.
  //  3.  If it's a non-PC relative instruction (e.g. beq) or a PC-relative instruction
  //      (e.g. jal) and bimodal counter status is strong/weak taken.
  //  4.  Fetch address' offset is less equal than the offset.
  val nlp_rdt_vld = s2_vld &&
                    !fbtb.io.resp.miss &&
                    (fbtb.io.resp.br_type || bim.io.resp.cnt > 1.U) &&
                    (s2_addr(log2Ceil(icacheParams.fetchBytes)-1, 0) <= fbtb.io.resp.offset)
  val do_stall = io.stall || icache.io.busy || scan.io.busy
  val do_kill = io.kill.valid

  //  Generate Enable Valid when
  //  1.  Stall signal is Low.
  //  2.  Global flush or BPD redirect Valid.
  //  3.  Next-Line Predictor redirect Valid.
  val gen_ena = !do_stall || do_kill || nlp_rdt_vld

  //  Next PC
  //  Priority:
  //  1.  Global Flush.
  //  2.  BPD Redirect.
  //  3.  Next-Line Predictor Redirect
  //  4.  Common.
  val next_pc = Mux(do_kill, io.kill.bits.addr,
    Mux(nlp_rdt_vld, fbtb.io.resp.tg_addr, core_pc + 16.U))

  //  Delay for eliminate half cycle
  when (state === s_idle) {
    state := s_gen
  }

  //  Stage 1 Valid when
  //  1.  State is s_gen.
  //  2.  Generate enable is Valid.
  s1_vld := (state === s_gen) && gen_ena
  s1_addr := core_pc
  when (s1_vld) {
    core_pc := next_pc
  }

  //  Bimodal
  bim.io.req.valid        := s1_vld
  bim.io.req.bits.addr    := s1_addr
  bim.io.upd.valid        := scan.io.nlp_upd.valid
  bim.io.upd.bits.addr    := scan.io.nlp_upd.bits.addr
  bim.io.upd.bits.taken   := scan.io.nlp_upd.bits.taken
  bim.io.upd.bits.old_cnt := scan.io.nlp_upd.bits.cnt

  //  FastBTB
  fbtb.io.req.valid         := s1_vld
  fbtb.io.req.bits.addr     := s1_addr
  fbtb.io.upd.valid         := scan.io.nlp_upd.valid
  fbtb.io.upd.bits.addr     := scan.io.nlp_upd.bits.addr
  fbtb.io.upd.bits.tg_addr  := scan.io.nlp_upd.bits.tg_addr
  fbtb.io.upd.bits.offset   := scan.io.nlp_upd.bits.offset
  fbtb.io.upd.bits.br_type  := scan.io.nlp_upd.bits.br_type

  //  Pipeline
  when (do_kill) {
    s2_vld := false.B
  } .elsewhen (do_stall) {
    s2_vld := s2_vld
    s2_addr:= s2_addr
    s3_cnt := s3_cnt
  } .otherwise {
    s2_vld := s1_vld
    s2_addr:= s1_addr
    s3_cnt := bim.io.resp.cnt
    s4_cnt := s3_cnt
  }

  //  **************************************
  //  Stage 2: ICache Read
  icache.io.hart_id       := io.hart_id
  icache.io.kill          := do_kill
  icache.io.stall         := scan.io.busy 
  icache.io.ptbr          := io.ptbr
  icache.io.prv           := io.prv
  icache.io.sfence        <> io.sfence
  icache.io.hart_id       := io.hart_id
  icache.io.req.valid     := s2_vld
  icache.io.req.bits.addr := s2_addr
  
  io.ptw <> icache.io.ptw
  io.mem <> icache.io.mem

  //  **************************************
  //  Stage 3: ICache Data Select
  io.perf.icache_access    := icache.io.icache_access
  io.perf.icache_miss      := icache.io.icache_miss
  io.perf.itlb_access      := icache.io.itlb_access
  io.perf.itlb_miss        := icache.io.itlb_miss

  //  **************************************
  //  Stage 4: Scanner
  scan.io.flush           := do_kill
  scan.io.stall           := fbuf.io.full || io.stall
  scan.io.prv             := io.prv
  scan.io.req.valid       := icache.io.resp.valid
  scan.io.req.bits.addr   := icache.io.resp.bits.addr
  scan.io.req.bits.data   := icache.io.resp.bits.data
  scan.io.req.bits.cause  := icache.io.resp.bits.cause
  scan.io.req.bits.cnt    := s4_cnt
  scan.io.upd             <> io.bpd_upd

  //  **************************************
  //  Stage 5: Fetch buffer
  fbuf.io.flush           := do_kill
  fbuf.io.stall           := io.stall
  fbuf.io.sync            := io.sync
  fbuf.io.req             <> scan.io.resp
  fbuf.io.bpd_info        <> scan.io.bpd_info

  io.resp := fbuf.io.resp
  //  End
}