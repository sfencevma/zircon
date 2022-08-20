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

class FetchBufferResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val inst      = UInt(instBits.W)
  val len       = Bool()
  val addr      = UInt(vaddrBits.W)
  val cause     = UInt(eLen.W)
  val taken     = Bool()
  val tg_addr   = UInt(vaddrBits.W)
}

class FetchBuffer(plWidth: Int, numEntries: Int)(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val stall = Input(Bool())
    val sync = Input(Bool())

    val req = Flipped(Valid(Vec(decodeWidth, new ScanStageResp)))
    val bpd_info = Input(new PredictorResp)
    val resp = Valid(Vec(decodeWidth, new FetchBufferResp))

    val full = Output(Bool())
  })

  class FetchBufferMeta extends Bundle {
    val inst      = UInt(instBits.W)
    val len       = Bool()
    val addr      = UInt(vaddrBits.W)
    val rl        = Bool()
    val aq        = Bool()
    val cause     = UInt(eLen.W)
    val taken     = Bool()
    val tg_addr   = UInt(vaddrBits.W)
  }

  def entrySz: Int = instBits + 3 + vaddrBits + eLen
  val ram = Mem(numEntries, new FetchBufferMeta)

  val idxBits = log2Ceil(numEntries) + 1
  val head = RegInit(UInt(idxBits.W), 0.U(idxBits.W))
  val tail = RegInit(UInt(idxBits.W), 0.U(idxBits.W))

  //
  val inst_nums = tail - head
  val nums = numEntries.U - inst_nums

  //  Enqueue
  val do_enq = !io.full && io.req.valid
  val enq_idxs = Wire(Vec(plWidth, UInt(idxBits.W)))
  val enq_insts = Wire(Vec(plWidth, new FetchBufferMeta))
  var enq_idx = tail
  for (w <- 0 until plWidth) {
    enq_idxs(w) := enq_idx
    enq_idx = enq_idx + io.req.bits(w).valid

    enq_insts(w).inst     := io.req.bits(w).inst
    enq_insts(w).len      := io.req.bits(w).len
    enq_insts(w).addr     := io.req.bits(w).addr
    enq_insts(w).rl       := io.req.bits(w).rl
    enq_insts(w).aq       := io.req.bits(w).aq
    enq_insts(w).cause    := io.req.bits(w).cause
    enq_insts(w).taken    := io.bpd_info.taken
    enq_insts(w).tg_addr  := io.bpd_info.tg_addr

    val idx = hashIdx(enq_idxs(w))
    when (do_enq) {
      ram(idx) := enq_insts(w)
    }

  }

  // Update PTR
  val enq_count = PopCount(io.req.bits.map(_.valid))
  when (io.flush) {
    tail := 0.U
  } .elsewhen (do_enq) {
    tail := tail + enq_count
  }

  io.full := nums < enq_count

  //
  val deq_idxs = Wire(Vec(plWidth, UInt(idxBits.W)))
  val deq_insts = Wire(Vec(plWidth, new FetchBufferMeta))
  var deq_idx = head
  for (w <- 0 until plWidth) {
    deq_idxs(w) := deq_idx
    deq_idx = deq_idx + 1.U

    val idx = hashIdx(deq_idxs(w))
    deq_insts(w) := ram(idx)
  }


  // RISC-V Weak Memory Ordering (RVWMO)
  //  1.  Release Ordering. Send SYNC instruction then stall.
  //  2.  Acquire Ordering. Send instruction then Stall.
  val deq_rl_constraints = Reverse(Cat(deq_insts.map(i => i.rl)))
  val deq_aq_constraints = Reverse(Cat(deq_insts.map(i => i.aq)))

  val s_normal::s_wait::Nil = Enum(2)
  val state_reg = RegInit(s_normal)
  val state = WireInit(state_reg)

  state := state_reg
  switch (state_reg) {
    is (s_normal) {
      when (deq_rl_constraints.orR | deq_aq_constraints.orR) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.sync) {
        state := s_normal
      }
    }
  }

  when (io.flush) {
    state_reg := s_normal
  } .elsewhen (io.sync) {
    state_reg := state
  } .elsewhen (io.stall) {
    state_reg := state_reg
  } .otherwise {
    state_reg := state
  }


  val deq_valids = Wire(Vec(plWidth, Bool()))
  val deq_aq_stall = Reg(Bool())
  val need_aq_stall = Wire(Bool())
  val resp = Reg(Valid(Vec(plWidth, new FetchBufferResp)))

  for (w <- 0 until plWidth) {
    val inst_valid = inst_nums > w.U
    deq_valids(w) := false.B
    when (state_reg === s_normal && !(io.stall | deq_aq_stall)) {
      if (w == 0) {
        deq_valids(w)       := inst_valid && !deq_rl_constraints(w)
        resp.bits(w).valid  := inst_valid
        resp.bits(w).inst   := Mux(deq_rl_constraints(w), INST_SYNC, deq_insts(w).inst)
      } else {
        deq_valids(w)       := inst_valid && !(deq_rl_constraints(w,0).orR | deq_aq_constraints(w-1, 0).orR)
        resp.bits(w).valid  := deq_valids(w)
        resp.bits(w).inst   := deq_insts(w).inst
      }
      resp.bits(w).len      := deq_insts(w).len
      resp.bits(w).addr     := deq_insts(w).addr
      resp.bits(w).cause    := deq_insts(w).cause
      resp.bits(w).taken    := deq_insts(w).taken
      resp.bits(w).tg_addr  := deq_insts(w).tg_addr
    }
  }

  need_aq_stall := deq_valids zip deq_aq_constraints.asBools map { case (v, aq) => v && aq } reduce (_|_)
  //  Update Condition
  when (io.flush) {
    deq_aq_stall := false.B
  } .elsewhen (io.sync) {
    deq_aq_stall := false.B
  } .elsewhen (state_reg === s_normal) {
    deq_aq_stall := need_aq_stall
  }
  when (io.sync) {
    ram(deq_idxs(0)).rl := false.B
  }

  //
  when (io.flush) {
    resp.valid := false.B
  } .elsewhen (io.stall) {
    resp.valid := resp.valid
  } .otherwise {
    resp.valid := PopCount(deq_valids) > 0.U && (state_reg === s_normal)
  }

  io.resp := resp

  //  Update PTR
  when (io.flush) {
    head := 0.U
  } .elsewhen (io.stall) {
    head := head
  } .otherwise {
    head := head + PopCount(deq_valids)
  }
}