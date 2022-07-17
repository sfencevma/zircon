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
package zircon.rob

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile.FPConstants
import freechips.rocketchip.rocket.Causes
import zircon.common._
import zircon.ifu._
import zircon.decode.bpd._
import zircon.csr._
import zircon.mmu._
import zircon.util._

class RobIssueReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid = Bool()
  val uop   = new MicroOp

  //  Branch information
  val is_jmp = Bool()
  val is_br = Bool()
  val is_call = Bool()
  val is_ret = Bool()
}

class RobRetireReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid     = Bool()
  val is_ld     = Bool()
  val is_st     = Bool()
  val ldst_vld  = Bool()
  val ldst_type = UInt(RT_SZ.W)
  val ldst_lreg = UInt(lregSz.W)
}

class RobExecReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid   = Bool()
  val rob_id  = UInt(robIdBits.W)
  val data    = UInt(xLen.W)
  val cause   = UInt(eLen.W)
  val fflags  = UInt(FPConstants.FLAGS_SZ.W)
}

class RobStoreExecReq(implicit p: Parameters) extends BaseZirconBundle {
  val st_id = UInt(stqIdBits.W)
}

class RobStoreExecResp(implicit p: Parameters) extends BaseZirconBundle {
  val flush = Bool()
  val cause = UInt(eLen.W)
}

class RobStoreExecIO(implicit p: Parameters) extends BaseZirconBundle {
  val req = Valid(new RobStoreExecReq)
  val resp = Flipped(Valid(new RobStoreExecResp))
}

class ROB(plWidth: Int, numIssuePorts: Int)(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
  with HasZirconCoreParameters {
  val io = IO(new Bundle() {
    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, new RobIssueReq)))
    val rets = Valid(Vec(plWidth, new RobRetireReq))

    //  Exceptions
    val interrupts = Decoupled(new Interrupts)
    val xpt = Valid(new Exceptions)
    val ext_interrupts = Flipped(Decoupled(new Interrupts))

    //  SFence
    val icache_sfence = Decoupled(new SFenceReq)
    val dcache_sfence = Decoupled(new SFenceReq)

    //  Performance
    val jump = Output(Bool())
    val br = Output(Bool())
    val csr = Output(Vec(plWidth, Bool()))
    val mul = Output(Vec(plWidth, Bool()))
    val div = Output(Vec(plWidth, Bool()))
    val fpu = Output(Vec(plWidth, Bool()))
    val ebreak = Output(Bool())
    val ecall = Output(Bool())

    //  Execute
    val exec_reqs = Vec(numIssuePorts, new RobExecReq)
    val store_exec = new RobStoreExecIO
    val bpd_upd = Valid(new PredictorUpdate)

    //  CSR
    val evec = Input(UInt(vaddrBits.W))
    val flush = Valid(new PCGenRediret)
    val stall = Input(Bool())

    val sync = Output(Bool())

    //
    val head = Input(UInt(robIdBits.W))
    val tail = Input(UInt(robIdBits.W))
  })

  //  Rob FSM
  val s_normal::s_int_chck::s_int_flush::s_wait::Nil = Enum(4)
  val rob_state = RegInit(s_normal)

  class RobMeta extends Bundle {
    val ld_id     = UInt(robIdBits.W)
    val st_id     = UInt(robIdBits.W)
    val uopc      = UInt(UOP_SZ.W)
    val len       = Bool()
    val ldst_vld  = Bool()
    val ldst_type = UInt(RT_SZ.W)
    val ldst_lreg = UInt(lregSz.W)
    val addr      = UInt(vaddrBits.W)
  }

  class RobCtrlFlags extends Bundle {
    val is_ld     = Bool()
    val is_st     = Bool()

    //  Branch information
    val is_jmp    = Bool()
    val is_br     = Bool()
    val is_call   = Bool()
    val is_ret    = Bool()

    val is_fence  = Bool()
    val is_sync   = Bool()

    //  PMU Part
    val is_csr    = Bool()
    val is_mul    = Bool()
    val is_div    = Bool()
    val is_fpu    = Bool()
    val is_branch = Bool()
    val is_jump   = Bool()

    //  Floating point flags
    val fflags    = UInt(FPConstants.FLAGS_SZ.W)
  }

  val meta_array  = Reg(Vec(numRobEntries, new RobMeta))
  val cfi_array   = Reg(Vec(numRobEntries, new RobCtrlFlags))
  val cause_array = Reg(Vec(numRobEntries, UInt(eLen.W)))
  val data_array  = Reg(Vec(numRobEntries, UInt(xLen.W)))
  val status_array = Reg(Vec(numRobEntries, Bool()))

  //  Enqueue
  val enq_reqs = WireInit(io.reqs.bits)
  for (w <- 0 until plWidth) {
    val idx = hashIdx(enq_reqs(w).uop.rob_id)
    when(io.reqs.valid && enq_reqs(w).valid) {
      meta_array(idx).ld_id       := enq_reqs(w).uop.ld_id
      meta_array(idx).st_id       := enq_reqs(w).uop.st_id
      meta_array(idx).uopc        := enq_reqs(w).uop.uopc
      meta_array(idx).len         := enq_reqs(w).uop.len
      meta_array(idx).ldst_vld    := enq_reqs(w).uop.ldst_vld
      meta_array(idx).ldst_type   := enq_reqs(w).uop.ldst_type
      meta_array(idx).ldst_lreg   := enq_reqs(w).uop.ldst_lreg
      meta_array(idx).addr        := enq_reqs(w).uop.addr

      //  Issue information
      cfi_array(idx).is_ld        := enq_reqs(w).uop.is_ld
      cfi_array(idx).is_st        := enq_reqs(w).uop.is_st

      //  Branch information
      cfi_array(idx).is_jmp       := enq_reqs(w).is_jmp
      cfi_array(idx).is_br        := enq_reqs(w).is_br
      cfi_array(idx).is_call      := enq_reqs(w).is_call
      cfi_array(idx).is_ret       := enq_reqs(w).is_ret

      //
      cfi_array(idx).is_fence     := isOneOf(enq_reqs(w).uop.uopc, Seq(UOP_SFENCE, UOP_FENCEI))
      cfi_array(idx).is_sync      := enq_reqs(w).uop.uopc === UOP_SYNC

      //  Performance information
      cfi_array(idx).is_csr       := enq_reqs(w).uop.is_csr
      cfi_array(idx).is_mul       := isOneOf(enq_reqs(w).uop.uopc, Seq(UOP_MUL, UOP_MULH, UOP_MULHSU, UOP_MULHSU))
      cfi_array(idx).is_div       := isOneOf(enq_reqs(w).uop.uopc, Seq(UOP_DIV, UOP_REM))
      cfi_array(idx).is_fpu       := isOneOf(enq_reqs(w).uop.port, Seq(PORT_FPU, PORT_FDIV))
      cfi_array(idx).is_jump      := enq_reqs(w).uop.uopc === UOP_JAL
      cfi_array(idx).is_branch    := enq_reqs(w).is_br

      //
      cause_array(idx)            := enq_reqs(w).uop.cause
      status_array(idx)           := false.B
    }
  }

  //  Execution Done
  for (i <- 0 until numIssuePorts) {
    val idx = hashIdx(io.exec_reqs(i).rob_id)
    when (io.exec_reqs(i).valid) {
      cause_array(idx)  := io.exec_reqs(i).cause
      data_array(idx)   := io.exec_reqs(i).data
      status_array(idx) := true.B
      cfi_array(idx)    := io.exec_reqs(i).fflags
    }
  }

  //  Retire
  val inst_nums = io.tail - io.head
  val ret_idxs = Wire(Vec(plWidth, UInt(robIdBits.W)))
  var ret_idx = io.head

  //  Step 1: Compute Retire Index and Check Constraints.
  val rob_valid_range = Reverse(Cat((0 until numRobEntries).map(idx => hashIdx(io.head) <= idx.U && hashIdx(io.tail) > idx.U)))
  val rob_valids_vec = Mux(io.head(robIdBits-1)^io.tail(robIdBits-1), ~rob_valid_range, rob_valid_range)
  val store_constraints_vec = Wire(Vec(plWidth, Bool()))
  val br_constraints_vec = Wire(Vec(plWidth, Bool()))
  val sfence_constraints_vec = Wire(Vec(plWidth, Bool()))
  val sync_constraints_vec = Wire(Vec(plWidth, Bool()))
  val xpt_constraints_vec = Wire(Vec(plWidth, Bool()))
  val exec_dones_vec = Wire(Vec(plWidth, Bool()))
  val csr_insts_vec = Wire(Vec(plWidth, Bool()))
  val fpu_insts_vec = Wire(Vec(plWidth, Bool()))
  val mul_insts_vec = Wire(Vec(plWidth, Bool()))
  val div_insts_vec = Wire(Vec(plWidth, Bool()))

  val store_constraints = Reverse(Cat(store_constraints_vec))
  val br_constraints = Reverse(Cat(br_constraints_vec))
  val xpt_constraints = Reverse(Cat(xpt_constraints_vec))
  val sfence_constraints = Reverse(Cat(sfence_constraints_vec))
  val exec_dones = Reverse(Cat(exec_dones_vec))

  val ret_valids = Wire(Vec(plWidth, Bool()))
  var ret_valid = exec_dones_vec.head

  for (w <- 0 until plWidth) {
    ret_idxs(w) := ret_idx
    ret_idx = ret_idx + 1.U

    //  Check Constraints
    val idx = hashIdx(ret_idxs(w))
    store_constraints_vec(w)  := cfi_array(idx).is_st
    br_constraints_vec(w)     := cfi_array(idx).is_branch | cfi_array(idx).is_jump
    xpt_constraints_vec(w)    := cause_array(idx).orR | isOneOf(meta_array(idx).uopc, Seq(UOP_EBREAK, UOP_ECALL))
    sfence_constraints_vec(w) := cfi_array(idx).is_fence
    sync_constraints_vec(w)   := cfi_array(idx).is_sync
    exec_dones_vec(w)         := status_array(idx)

    csr_insts_vec(w)          := cfi_array(idx).is_csr
    mul_insts_vec(w)          := cfi_array(idx).is_mul
    div_insts_vec(w)          := cfi_array(idx).is_div
    fpu_insts_vec(w)          := cfi_array(idx).is_fpu

    //  Step 2: Compute Retire Valid signal
    //  Retire Valid when
    //  1.  Execute Done
    //  2.  Has no br, store, xpt, sfence before
    //  3.  Stall is low
    ret_valids(w) := ret_valid
    ret_valid = rob_valids_vec(idx) &
      status_array(idx) &
      !(store_constraints(w, 0).orR |
        br_constraints(w, 0).orR |
        xpt_constraints(w, 0).orR |
        sfence_constraints(w, 0).orR) &
      !io.stall &
      (rob_state === s_normal)

    //  Step 3: Deallocate (if need)
    io.rets.bits(w).valid := ret_valids(w)
    io.rets.bits(w).is_ld := cfi_array(idx).is_ld
    io.rets.bits(w).is_st := cfi_array(idx).is_st
  }

  val ret_count = PopCount(ret_valids)
  io.rets.valid := ret_count > 0.U

  //  Step 4: Check if has exceptions
  val xpt_detected = ret_valids.head & xpt_constraints_vec.head
  val xpt_idx = hashIdx(io.head)
  val xpt_flush_valid = RegNext(xpt_detected)
  val xpt_flush_addr = RegNext(io.evec)

  val xpt_array = meta_array(xpt_idx)
  val xpt_valid = xpt_constraints_vec.head && isOneOf(cause_array(xpt_idx), Seq(
    Causes.breakpoint.U,
    Causes.misaligned_load.U,
    Causes.misaligned_store.U,
    Causes.load_access.U,
    Causes.store_access.U,
    Causes.fetch_access.U,
    Causes.load_page_fault.U,
    Causes.store_page_fault.U,
    Causes.fetch_page_fault.U
  ))

  io.xpt.valid      := xpt_detected
  io.xpt.bits.ecall := xpt_array.uopc === UOP_ECALL
  io.xpt.bits.ebreak:= xpt_array.uopc === UOP_EBREAK
  io.xpt.bits.mret  := xpt_array.uopc === UOP_MRET
  io.xpt.bits.sret  := xpt_array.uopc === UOP_SRET
  io.xpt.bits.uret  := xpt_array.uopc === UOP_URET
  io.xpt.bits.wfi   := xpt_array.uopc === UOP_WFI
  io.xpt.bits.xpt   := xpt_valid
  io.xpt.bits.cause := cause_array(xpt_idx)
  io.xpt.bits.addr  := xpt_array.addr
  io.xpt.bits.tval  := Mux(xpt_valid, xpt_array.addr, 0.U)

  val s_fence_idle::s_fence_req::s_fence_wait1::s_fence_wait2::Nil = Enum(4)
  val fence_state = RegInit(s_fence_idle)
  val fence_idx = hashIdx(io.head)
  val fence_detected = ret_valids.head & sfence_constraints_vec.head & !xpt_detected
  val fence_valid = isOneOf(fence_state, Seq(s_fence_req, s_fence_idle))
  io.icache_sfence.valid := RegNext((fence_detected & fence_valid) || (fence_state === s_fence_wait2))
  io.icache_sfence.bits.vpn_vld  := 0.U
  io.icache_sfence.bits.vpn      := 0.U
  io.icache_sfence.bits.asid_vld := 0.U
  io.icache_sfence.bits.asid     := 0.U
  io.dcache_sfence.valid         := RegNext((fence_detected && fence_valid) || (fence_state === s_fence_wait1))
  io.dcache_sfence.bits          := io.icache_sfence.bits

  val fence_done = (io.icache_sfence.ready & io.dcache_sfence.ready) |
    (io.icache_sfence.ready & (fence_state === s_fence_wait2)) |
    (io.dcache_sfence.ready & (fence_state === s_fence_wait1))

  switch (fence_state) {
    is (s_fence_idle) {
      when (fence_detected) {
        fence_state := s_fence_req
      }
    }
    is (s_fence_req) {
      when (io.icache_sfence.ready && io.dcache_sfence.ready) {
        fence_state := s_fence_idle
      } .elsewhen (io.icache_sfence.ready) {
        fence_state := s_fence_wait1
      } .elsewhen (io.dcache_sfence.ready) {
        fence_state := s_fence_wait2
      }
    }
    is (s_fence_wait1) {
      when (io.dcache_sfence.ready) {
        fence_state := s_fence_req
      }
    }
    is (s_fence_wait2) {
      when (io.icache_sfence.ready) {
        fence_state := s_fence_req
      }
    }
  }

  //  Update flag
  when (fence_done) {
    cfi_array(fence_idx).is_fence := false.B
  }


  //  Step 4: Check if store instruction
  val s_store_idle::s_store_req::s_store_wait::s_store_flush::Nil = Enum(3)
  val store_state = RegInit(s_store_idle)
  val store_idx = hashIdx(io.head)
  val store_detected = ret_valids.head & store_constraints_vec.head & !xpt_detected
  val store_done = io.store_exec.resp.valid

  io.store_exec.req.valid := RegNext(store_state === s_store_req)
  io.store_exec.req.bits.st_id := meta_array(store_idx).st_id

  switch (store_state) {
    is (s_store_idle) {
      when (store_detected) {
        store_state := s_store_req
      }
    }
    is (s_store_req) {
      store_state := s_store_wait
    }
    is (s_store_wait) {
      when (io.store_exec.resp.valid) {
        store_state := Mux(io.store_exec.resp.bits.flush, s_store_flush, s_store_idle)
      }
    }
    is (s_store_flush) {
      store_state := s_store_idle
    }
  }

  when (io.store_exec.resp.valid && !io.store_exec.resp.bits.flush) {
    cfi_array(store_idx).is_st := false.B
    cause_array(store_idx) := io.store_exec.resp.bits.cause
  }

  val store_flush_valid = store_state === s_store_flush
  val store_flush_addr  = meta_array(store_idx).addr

  //  Step 4: Check if Branch
  val s_br_idle::s_br_read::s_br_upd::Nil = Enum(3)
  val br_state = RegInit(s_br_idle)
  val br_idx = hashIdx(io.head)
  val br_detected = ret_valids.head & br_constraints_vec.head & !xpt_detected
  val br_done = br_state === s_br_upd

  switch (br_state) {
    is (s_br_idle) {
      when (br_detected) {
        br_state := s_br_read
      }
    }
    is (s_br_read) {
      br_state := s_br_upd
    }
    is (s_br_upd) {
      br_state := s_br_idle
    }
  }

  when (s_br_upd === br_state) {
    cfi_array(br_idx).is_branch := false.B
    cfi_array(br_idx).is_jump := false.B
  }

  //  Update BPD
  io.bpd_upd.valid        := br_state === s_br_read
  io.bpd_upd.bits.addr    := meta_array(br_idx).addr
  io.bpd_upd.bits.taken   := data_array(br_idx)(xLen-1)
  io.bpd_upd.bits.tg_addr := data_array(br_idx)(vaddrBits-1,0)
  io.bpd_upd.bits.is_jmp  := cfi_array(br_idx).is_jmp
  io.bpd_upd.bits.is_br   := cfi_array(br_idx).is_br
  io.bpd_upd.bits.is_ret  := cfi_array(br_idx).is_ret
  io.bpd_upd.bits.is_call := cfi_array(br_idx).is_call

  //  Rob FSM
  val need_wait = (fence_detected | store_detected | br_detected)
  switch (rob_state) {
    is (s_normal) {
      when (need_wait) {
        rob_state := s_wait
      } .otherwise {
        rob_state := s_int_chck
      }
    }
    is (s_wait) {
      when (fence_done | store_done | br_done) {
        rob_state := s_normal
      }
    }
    is (s_int_chck) {
      when (io.ext_interrupts.valid) {
        rob_state := s_int_flush
      } .otherwise {
        rob_state := s_normal
      }
    }
    is (s_int_flush) {
      rob_state := s_normal
    }
  }

  io.interrupts.valid := (rob_state === s_int_chck) & io.ext_interrupts.valid
  io.interrupts.bits := io.ext_interrupts.bits
  io.ext_interrupts.ready := io.interrupts.ready

  val intr_flush_valid = rob_state === s_int_flush
  val intr_flush_addr = RegNext(io.evec)
  io.flush.valid := xpt_flush_valid || store_flush_valid || intr_flush_valid
  io.flush.bits.addr := Mux(xpt_flush_valid, xpt_flush_addr,
    Mux(store_flush_valid, store_flush_addr, intr_flush_addr))

  //  Performance
  val ret_metas = ret_idxs.map(idx => meta_array(hashIdx(idx)))
  val ret_cfis = ret_idxs.map(idx => cfi_array(hashIdx(idx)))
  io.jump   := ret_valids.head & ret_cfis.head.is_jump
  io.br     := ret_valids.head & ret_cfis.head.is_br
  io.csr    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_csr }
  io.mul    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_mul }
  io.div    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_div }
  io.fpu    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_fpu }
  io.ecall  := ret_valids.head & (ret_metas.head.uopc === UOP_ECALL)
  io.ebreak := ret_valids.head & (ret_metas.head.uopc === UOP_EBREAK)
  io.sync   := ret_valids zip ret_cfis map { case (v, i) => v & i.is_sync }
}