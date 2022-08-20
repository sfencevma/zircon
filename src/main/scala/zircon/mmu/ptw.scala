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
import freechips.rocketchip.rocket.Causes
import zircon.common._
import zircon.frontend._
import difftest._
import zircon.exu._
import zircon.utils._

class PTWReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class PTWResp(implicit p: Parameters) extends BaseZirconBundle {
  val pte   = new PTE
  val level = UInt(lgPgLevels.W)
  val cause = UInt(eLen.W)
}

class PtwMemReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class PtwMemResp(implicit p: Parameters) extends BaseZirconBundle {
  val data  = UInt(xLen.W)
  val cause = UInt(eLen.W)
}

class PtwMemIO(implicit p: Parameters) extends BaseZirconBundle {
  val req = Decoupled(new MemReq(xLen))
  val resp = Flipped(Decoupled(new MemResp(xLen)))
}

class PTW(implicit p: Parameters) extends BaseZirconModule with MemoryOpConstants {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val ptbr = Input(new PTBR)
    val prv = Input(UInt(2.W))

    val tsr = Input(Bool())
    val sum = Input(Bool())
    val mprv = Input(Bool())

    val itlb_req    = Flipped(Decoupled(new ITLBPTWReq))
    val itlb_resp   = Decoupled(new ITLBPTWResp)

    val dtlb_req    = Flipped(Decoupled(new DTLBPTWReq))
    val dtlb_resp   = Decoupled(new DTLBPTWResp)

    val mem = new PtwMemIO
  })

  //  FSM
  val s_ready::s_request::s_wait::s_fragment_superpage::s_wait2::Nil = Enum(5)
  val state = RegInit(s_ready)

  val arb = Module(new RRArbiter(new PTWReq, 2))
  arb.io.in(0) <> io.itlb_req
  arb.io.in(1) <> io.dtlb_req
  arb.io.out.ready := state === s_ready

  val chosen = RegInit(UInt(), 0.U)
  when (arb.io.out.fire) {
    chosen := arb.io.chosen
  }

  //
  val lvl_count = Reg(UInt(log2Up(pgLevels).W))

  val r_req = Reg(new PTWReq)
  val pte_reg = Reg(new PTE)

  val (pte, invalid_addr) = {
    val tmp = WireInit(io.mem.resp.bits.data).asTypeOf(new PTE)
    val res = WireInit(tmp)
    res.ppn := tmp.ppn(ppnBits-1, 0)
    when (tmp.r || tmp.w || tmp.x) {
      for (i <- 0 until pgLevels-1) {
        when (lvl_count <= i.U &&
          tmp.ppn((pgLevels-1-i)*pgLevelBits-1, (pgLevels-2-i)*pgLevelBits) =/= 0.U) {
          res.v := false.B
        }
      }
    }
    (res, (tmp.ppn >> ppnBits).asUInt =/= 0.U)
  }

  val traverse = pte.table && !invalid_addr && lvl_count < (pgLevels-1).U
  val pte_addr = {
    val vpn_idxs = (0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1) * pgLevelBits)(pgLevelBits-1,0))
    val vpn_idx = VecInit(vpn_idxs)(lvl_count)
    Cat(pte_reg.ppn, vpn_idx) << log2Ceil(xLen/8)
  }
  val superpage_ppn = {
    val choices = (pgLevels-1 until 0 by -1).map(i =>
      Cat(pte_reg.ppn >> (pgLevelBits * i), r_req.addr(pgLevelBits*i-1, 0))
    )
    VecInit(choices)(lvl_count)
  }

  def makePTE(x: UInt, d: PTE) = {
    val pte = WireInit(d)
    pte.ppn := x
    pte
  }

  //  Buffer
  val req_fire = arb.io.out.fire 
  when (req_fire) {
    r_req := arb.io.out.bits 
  }

  pte_reg := Mux(io.mem.resp.fire, pte,
    Mux(state === s_fragment_superpage, makePTE(superpage_ppn, pte_reg),
      Mux(req_fire, makePTE(io.ptbr.ppn, pte_reg), pte_reg)))


  val resp_valid = Reg(Bool())
  val cause = Reg(UInt(eLen.W))

  //  Set Default
  resp_valid := false.B

  when (req_fire && state === s_ready) {
    state := s_request
    lvl_count := pgLevels.U - minPgLevels.U - io.ptbr.mode(log2Ceil(pgLevels-minPgLevels+1)-1, 0)
    cause := 0.U
  }
  when (state === s_request) {
    when (io.mem.req.fire) { state := s_wait }
    when (io.kill) { state := s_ready }
  }
  when (state === s_fragment_superpage) {
    resp_valid := true.B
    state := s_ready
  }
  when (state === s_wait) {
    when (io.mem.resp.fire) {
      when(io.mem.resp.bits.cause.orR) {
        state := s_ready
        resp_valid := true.B
        cause := io.mem.resp.bits.cause
      }.elsewhen(traverse) {
        state := s_request
        lvl_count := lvl_count + 1.U
      }.otherwise {
        val access_xpt = pte.v && invalid_addr
        when (lvl_count =/= (pgLevels - 1).U && !access_xpt) {
          state := s_fragment_superpage
          lvl_count := lvl_count - 1.U
        } .otherwise {
          state := Mux(io.itlb_resp.fire || io.dtlb_resp.fire, s_ready, state)
          resp_valid := true.B
          cause := Mux(access_xpt, Causes.load_page_fault.U, 0.U)
        }
      }
    }
  }

  //  Memory Require
  io.mem.req.valid      := state === s_request
  io.mem.req.bits.addr  := r_req.addr
  io.mem.req.bits.cmd   := M_XRD
  io.mem.req.bits.len   := 1.U
  io.mem.req.bits.size  := memBusParams.SIZE_8B
  io.mem.req.bits.data  := DontCare
  io.mem.resp.ready     := Mux(chosen === 0.U, io.itlb_resp.ready, io.dtlb_resp.ready) 

  //  Response
  //  ITLB
  io.itlb_resp.valid      := chosen === 0.U & resp_valid
  io.itlb_resp.bits.pte   := pte_reg
  io.itlb_resp.bits.level := lvl_count
  io.itlb_resp.bits.cause := cause

  //  DTLB
  io.dtlb_resp.valid      := chosen === 1.U & resp_valid
  io.dtlb_resp.bits.pte   := pte_reg
  io.dtlb_resp.bits.level := lvl_count
  io.dtlb_resp.bits.cause := cause


 // End
}