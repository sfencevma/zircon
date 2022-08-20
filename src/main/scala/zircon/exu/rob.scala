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
import freechips.rocketchip.rocket.Causes
import freechips.rocketchip.tile.FPConstants
import zircon.common._
import zircon.frontend._
import zircon.mmu._
import zircon.utils._
import difftest._

class RobIssueReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid     = Bool()
  val uop       = new RobMicroOp
}

class RobRetireReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  //  Control
  val valid     = Bool()

  //  Update Rename Table
  val rob_id    = UInt(robIdBits.W)

  //  Update Allocator
  val is_ld     = Bool()
  val is_st     = Bool()

  //  Write Back
  val ldst_vld  = Bool()
  val ldst_type = UInt(RT_SZ.W)
  val ldst_lreg = UInt(lregSz.W)
  val ldst      = UInt(xLen.W)
  val csr_addr  = UInt(csrAddrBits.W)
}

class RobExecReq(hasALU: Boolean = false,
                 hasBRU: Boolean = false,
                 hasFPU: Boolean = false)(implicit p: Parameters) extends BaseZirconBundle {
  val valid   = Bool()
  val rob_id  = UInt(robIdBits.W)
  val data    = UInt(xLen.W)
  val cause   = UInt(eLen.W)

  //  Option Part.
  val taken   = if (hasBRU) Bool() else null
  val tg_addr = if (hasBRU) UInt(vaddrBits.W) else null
  val vpn_vld = if (hasALU) Bool() else null
  val vpn     = if (hasALU) UInt(vpnBits.W) else null
  val asid_vld= if (hasALU) Bool() else null
  val asid    = if (hasALU) UInt(asIdBits.W) else null
  val fflags  = if (hasFPU) UInt(FPConstants.FLAGS_SZ.W) else null
}

class RobStoreExecReq(implicit p: Parameters) extends BaseZirconBundle {
  val st_id   = UInt(stqIdBits.W)
  val rob_id  = UInt(robIdBits.W)
}

class RobStoreExecResp(implicit p: Parameters) extends BaseZirconBundle {
  val flush = Bool()
  val addr  = UInt(vaddrBits.W)
  val cause = UInt(eLen.W)
}

class RobStoreExecIO(implicit p: Parameters) extends BaseZirconBundle {
  val req   = Decoupled(new RobStoreExecReq)
  val resp  = Flipped(Decoupled(new RobStoreExecResp))
}

class RobPerfs(plWidth: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val jump    = Output(Bool())
  val br      = Output(Bool())
  val csr     = Output(Bool())
  val mul     = Output(Vec(plWidth, Bool()))
  val div     = Output(Vec(plWidth, Bool()))
  val fpu     = Output(Vec(plWidth, Bool()))
  val ebreak  = Output(Bool())
  val ecall   = Output(Bool())
  val mispred = Output(Bool())
}

class ROB(plWidth: Int, numIssuePorts: Int, numRobReadPorts: Int)(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
  with HasZirconCoreParameters
{

  val io = IO(new Bundle() {
    //  
    val hart_id = Input(UInt(64.W))
    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, new RobIssueReq)))
    val rets = Valid(Vec(plWidth, new RobRetireReq))

    //  Exceptions
    val interrupts      = Output(new Interrupts)
    val xpt             = Valid(new Exceptions)
    val ext_interrupts  = Input(new Interrupts)

    //  SFence
    val icache_sfence = Decoupled(new SFenceReq)
    val dcache_sfence = Decoupled(new SFenceReq)

    //  Performance
    val perf = Output(new RobPerfs(plWidth))

    //  Execute
    // val exec_reqs   = Input(Seq(new RobExecReq(hasALU = true),  //  ALU Port
    //                             new RobExecReq(hasBRU = true),  //  BRU Port
    //                             new RobExecReq(),               //  MUL Port
    //                             new RobExecReq(),               //  DIV Port
    //                             new RobExecReq()))              //  LSU Port
    val alu_req = Input(new RobExecReq(hasALU = true))
    val bru_req = Input(new RobExecReq(hasBRU = true))
    val mul_req = Input(new RobExecReq())
    val div_req = Input(new RobExecReq())
    val lsu_req = Input(new RobExecReq())

    // val fdiv_req= Input(new RobExecReq(hasFPU = true))
    // val fpu_req = Input(new RobExecReq(hasFPU = true))
    val store_exec  = new RobStoreExecIO
    val bpd_upd     = Decoupled(new PredictorUpdate)
    val write_port  = Flipped(new CsrWritePortIO) //  CSR

    //  Issue Read
    val read_ports = Vec(numRobReadPorts, new IssueReadRobPortIO)

    //  CSR
    val xpt_catch = Input(Bool())
    val evec  = Input(UInt(vaddrBits.W))
    val kill = Valid(new FrontendRedirect)
    val stall = Input(Bool())

    val sync  = Output(Bool())
    //
    val head = Input(UInt(robIdBits.W))
    val tail = Input(UInt(robIdBits.W))
  })

  val exec_reqs = Seq(io.alu_req, io.bru_req, io.mul_req, io.div_req, io.lsu_req)

  //  Rob FSM
  //  State description:
  //  s_normal      : Normal retire.
  //  s_int_chck    : Check whether interrupts pending.
  //  s_int_flush   : Flush pipeline by interrupts.
  //  s_wait        : Wait for special retire.
  val s_normal::s_int_chck::s_int_flush::s_wait::Nil = Enum(4)
  val state = RegInit(s_normal)

  class RobMeta extends Bundle {
    val rob_id    = UInt(robIdBits.W)
    val ld_id     = UInt(ldqIdBits.W)
    val st_id     = UInt(stqIdBits.W)

    val uopc      = UInt(UOP_SZ.W)
    val len       = Bool()

    val ldst_vld  = Bool()
    val ldst_type = UInt(RT_SZ.W)
    val ldst_lreg = UInt(lregSz.W)
    val addr      = UInt(vaddrBits.W)
    val csr_addr  = UInt(csrAddrBits.W)

    val taken     = Bool()
    val tg_addr   = UInt(vaddrBits.W)
  }

  class RobCtrlFlags extends Bundle {
    // val is_ld     = Bool()
    //  Branch information
    val is_jmp    = Bool()
    val is_br     = Bool()
    val is_call   = Bool()
    val is_ret    = Bool()

    //  Control
    val is_st     = Bool()
    val is_ld     = Bool()
    val is_branch = Bool()
    val is_fence  = Bool()
    val is_sync   = Bool()
    val is_sys    = Bool()

    //  Performance
    val is_csr    = Bool()
    val is_mul    = Bool()
    val is_div    = Bool()
    val is_fpu    = Bool()

    //  FENCE
    val vpn_vld   = Bool()
    val vpn       = UInt(vpnBits.W)
    val asid_vld  = Bool()
    val asid      = UInt(asIdBits.W)

    //  Floating point flags
    val fflags    = UInt(FPConstants.FLAGS_SZ.W)
  }

  class RobData extends Bundle {
    //  Fence
    val vpn_vld = Bool()
    val vpn     = UInt(vpnBits.W)
    val asid_vld= Bool()
    val asid    = UInt(asIdBits.W)

    //  Prediction
    val taken   = Bool()
    val tg_addr = UInt(vaddrBits.W)

    //  Data
    val data    = UInt(xLen.W)
  }

  val meta_array    = Reg(Vec(numRobEntries, new RobMeta))
  val cfi_array     = Reg(Vec(numRobEntries, new RobCtrlFlags))
  val cause_array   = Reg(Vec(numRobEntries, UInt(eLen.W)))
  val data_array    = Reg(Vec(numRobEntries, new RobData))
  val status_array  = Reg(Vec(numRobEntries, Bool()))

  val enq_reqs = WireInit(io.reqs.bits)
  for (w <- 0 until plWidth) {
    val idx = hashIdx(enq_reqs(w).uop.rob_id)
    when (io.reqs.valid & enq_reqs(w).valid) {
      meta_array(idx).rob_id      := enq_reqs(w).uop.rob_id
      meta_array(idx).ld_id       := enq_reqs(w).uop.ld_id
      meta_array(idx).st_id       := enq_reqs(w).uop.st_id
      meta_array(idx).uopc        := enq_reqs(w).uop.uopc
      meta_array(idx).len         := enq_reqs(w).uop.len
      meta_array(idx).ldst_vld    := enq_reqs(w).uop.ldst_vld
      meta_array(idx).ldst_type   := enq_reqs(w).uop.ldst_type
      meta_array(idx).ldst_lreg   := enq_reqs(w).uop.ldst_lreg
      meta_array(idx).csr_addr    := enq_reqs(w).uop.csr_addr
      meta_array(idx).addr        := enq_reqs(w).uop.addr
      meta_array(idx).taken       := enq_reqs(w).uop.taken
      meta_array(idx).tg_addr     := enq_reqs(w).uop.tg_addr

      //  Issue information
      cfi_array(idx).is_st        := enq_reqs(w).uop.is_st
      cfi_array(idx).is_ld        := enq_reqs(w).uop.is_ld
      cfi_array(idx).is_fence     := isOneOf(enq_reqs(w).uop.uopc, Seq(UOP_SFENCE, UOP_FENCEI))
      cfi_array(idx).is_sync      := enq_reqs(w).uop.uopc === UOP_SYNC
      cfi_array(idx).is_branch    := enq_reqs(w).uop.is_br  |
                                     enq_reqs(w).uop.is_jmp |
                                     enq_reqs(w).uop.is_call|
                                     enq_reqs(w).uop.is_ret
      cfi_array(idx).is_sys       := enq_reqs(w).uop.is_csr

      //  Branch information
      cfi_array(idx).is_jmp       := enq_reqs(w).uop.is_jmp
      cfi_array(idx).is_br        := enq_reqs(w).uop.is_br
      cfi_array(idx).is_call      := enq_reqs(w).uop.is_call
      cfi_array(idx).is_ret       := enq_reqs(w).uop.is_ret

      //  Performance
      cfi_array(idx).is_csr       := enq_reqs(w).uop.is_csr
      cfi_array(idx).is_div       := enq_reqs(w).uop.port === PORT_DIV
      cfi_array(idx).is_fpu       := isOneOf(enq_reqs(w).uop.port, Seq(PORT_FPU, PORT_FDIV))

      //  Floating Point FFlags
      cfi_array(idx).fflags       := 0.U  //  Clean

      //
      cause_array(idx)            := enq_reqs(w).uop.cause
      status_array(idx)           := false.B
    }
  }

  //  Execution Done
  for (p <- 0 until numIssuePorts) {
    val idx = hashIdx(exec_reqs(p).rob_id)
    when (exec_reqs(p).valid) {
      status_array(idx)     := true.B
      cause_array(idx)      := exec_reqs(p).cause
      data_array(idx).data  := exec_reqs(p).data

      if (ALU_PORT == p) { //  ALU
        data_array(idx).vpn_vld := exec_reqs(p).vpn_vld
        data_array(idx).vpn     := exec_reqs(p).vpn
        data_array(idx).asid_vld:= exec_reqs(p).asid_vld
        data_array(idx).asid    := exec_reqs(p).asid
      } else if (BRU_PORT == p) {  //  BRU
        data_array(idx).taken   := exec_reqs(p).taken
        data_array(idx).tg_addr := exec_reqs(p).tg_addr
      } else if ((FDIV_PORT == p) || (FPU_PORT == p)) {
        cfi_array(idx).fflags := exec_reqs(p).fflags
      }
    }
  }



  //  Step 1: Compute retire index and check constraints.
  val rob_valid_range = Reverse(Cat((0 until numRobEntries).map(idx => hashIdx(io.head) <= idx.U && hashIdx(io.tail) > idx.U)))
  val rob_valids = Mux(io.head(robIdBits-1)^io.tail(robIdBits-1), ~rob_valid_range, rob_valid_range)
  val can_retire = !io.stall

  //  Constraints
  val store_constraints_vec   = Wire(Vec(plWidth, Bool()))
  val br_constraints_vec      = Wire(Vec(plWidth, Bool()))
  val fence_constraints_vec   = Wire(Vec(plWidth, Bool()))
  val sync_constraints_vec    = Wire(Vec(plWidth, Bool()))
  val xpt_constraints_vec     = Wire(Vec(plWidth, Bool()))
  val csr_constraints_vec     = Wire(Vec(plWidth, Bool()))

  //
  val exec_dones_vec  = Wire(Vec(plWidth, Bool()))
  val fpu_insts_vec   = Wire(Vec(plWidth, Bool()))
  val mul_insts_vec   = Wire(Vec(plWidth, Bool()))
  val div_insts_vec   = Wire(Vec(plWidth, Bool()))

  val store_constraints   = Reverse(Cat(store_constraints_vec))
  val br_constraints      = Reverse(Cat(br_constraints_vec))
  val xpt_constraints     = Reverse(Cat(xpt_constraints_vec))
  val fence_constraints   = Reverse(Cat(fence_constraints_vec))
  val csr_constraints     = Reverse(Cat(csr_constraints_vec))
  val exec_dones          = Reverse(Cat(exec_dones_vec))

  //  Retire
  val ret_valids  = Wire(Vec(plWidth, Bool()))
  var ret_valid   = rob_valids(hashIdx(io.head)) && exec_dones_vec.head
  val ret_idxs = Wire(Vec(plWidth, UInt(robIdBits.W)))
  var ret_idx = io.head

  for (w <- 0 until plWidth) {
    ret_idxs(w) := ret_idx
    ret_idx = ret_idx + 1.U

    //  Check Constraints.
    val idx = hashIdx(ret_idxs(w))
    store_constraints_vec(w)  := cfi_array(idx).is_st
    br_constraints_vec(w)     := cfi_array(idx).is_branch
    xpt_constraints_vec(w)    := cause_array(idx).orR |
                                  isOneOf(meta_array(idx).uopc, Seq(UOP_EBREAK, UOP_ECALL))
    fence_constraints_vec(w)  := cfi_array(idx).is_fence
    sync_constraints_vec(w)   := cfi_array(idx).is_sync
    csr_constraints_vec(w)    := cfi_array(idx).is_sys

    //  Execute
    exec_dones_vec(w)         := status_array(idx)
    mul_insts_vec(w)          := cfi_array(idx).is_mul
    div_insts_vec(w)          := cfi_array(idx).is_div
    fpu_insts_vec(w)          := cfi_array(idx).is_fpu

    //  Step 2: Compute Retire Valid signal
    //  Retire Valid when
    //  1.  Entry Valid.
    //  2.  Execute Done.
    //  3.  Has no br, store, xpt, fence before.
    //  4.  Stall is low.
    //  5.  State is Normal.
    ret_valids(w) := ret_valid
    ret_valid = rob_valids(idx) &
                status_array(idx) &
                !(store_constraints(w,0).orR |
                  br_constraints(w,0).orR |
                  xpt_constraints(w,0).orR |
                  fence_constraints(w,0).orR |
                  csr_constraints(w, 0).orR) &
                !io.stall &
                (state === s_normal)

    //  Step 3: Deallocate (if need).
    io.rets.bits(w).valid     := ret_valids(w)
    io.rets.bits(w).rob_id    := meta_array(idx).rob_id
    io.rets.bits(w).is_ld     := cfi_array(idx).is_ld
    io.rets.bits(w).is_st     := cfi_array(idx).is_st
    io.rets.bits(w).ldst_vld  := meta_array(idx).ldst_vld
    io.rets.bits(w).ldst_type := meta_array(idx).ldst_type
    io.rets.bits(w).ldst_lreg := meta_array(idx).ldst_lreg
    io.rets.bits(w).ldst      := data_array(idx).data
    io.rets.bits(w).csr_addr  := meta_array(idx).csr_addr
  }

  val ret_count = PopCount(ret_valids)
  io.rets.valid := ret_count > 0.U

  //  Step 4: Check whether has exceptions
  //  State description:
  //  s_xpt_idle        : IDLE, do nothing.
  //  s_xpt_req         : Exception Require.
  //  s_xpt_flush       : Exception Flush.
  //  s_xpt_done        : Exception Done.
  val s_xpt_idle::s_xpt_req::s_xpt_flush::s_xpt_done::Nil = Enum(4)
  val xpt_state = RegInit(s_xpt_idle)
  val xpt_idx = hashIdx(io.head)

  val xpt_array = meta_array(xpt_idx)
  //  Exception Valid when
  //  1.  Retire Valid.
  //  2.  Has exception.
  //  3.  Support exception.
  val xpt_valid = ret_valids.head & xpt_constraints_vec.head & isOneOf(cause_array(xpt_idx), Seq(
    Causes.illegal_instruction.U,
    Causes.breakpoint.U,
    Causes.misaligned_fetch.U, 
    Causes.misaligned_load.U,
    Causes.misaligned_store.U,
    Causes.fetch_access.U,
    Causes.load_access.U,
    Causes.store_access.U,
    Causes.fetch_page_fault.U,
    Causes.load_page_fault.U,
    Causes.store_page_fault.U,
    Causes.machine_ecall.U,
    Causes.supervisor_ecall.U
  ))
  val xpt_done = xpt_state === s_xpt_done

  switch (xpt_state) {
    //  Step 1: Detect exception.
    is (s_xpt_idle) {
      when (xpt_valid & can_retire) {
        xpt_state := s_xpt_req
      }
    }

    //  Step 2: Exception Require
    is (s_xpt_req) {
      xpt_state := s_xpt_flush
    }

    //  Step 3: Exception Flush
    is (s_xpt_flush) {
      xpt_state := s_xpt_done
    }

    //  Step 4: Exception Success.
    is (s_xpt_done) {
      xpt_state := s_xpt_idle
    }
  }

  //  Update flag.
  when (xpt_done) {
    //  Clean cause code.
    cause_array(xpt_idx) := 0.U
  }

  //  Rob to CSRs.
  io.xpt.valid       := xpt_state === s_xpt_req
  io.xpt.bits.ecall  := xpt_array.uopc === UOP_ECALL
  io.xpt.bits.ebreak := xpt_array.uopc === UOP_EBREAK
  io.xpt.bits.mret   := xpt_array.uopc === UOP_MRET
  io.xpt.bits.sret   := xpt_array.uopc === UOP_SRET
  io.xpt.bits.uret   := xpt_array.uopc === UOP_URET
  io.xpt.bits.wfi    := xpt_array.uopc === UOP_WFI
  io.xpt.bits.xpt    := xpt_valid
  io.xpt.bits.cause  := cause_array(xpt_idx)
  io.xpt.bits.addr   := xpt_array.addr
  io.xpt.bits.tval   := Mux(xpt_valid, xpt_array.addr, 0.U)

  //  Step 4.1 : Check whether has fence.
  //  State description:
  //  s_fence_idle      : IDLE, do nothing.
  //  s_fence_req       : FENCE Require.
  //  s_fence_wait1     : Wait for icache flush.
  //  s_fence_wait2     : Wait for dcache flush.
  //  s_fence_done      : FENCE flush done.
  val s_fence_idle::s_fence_req::s_fence_wait1::s_fence_wait2::s_fence_done::Nil = Enum(5)
  val fence_state = RegInit(s_fence_idle)
  val fence_idx = hashIdx(io.head)

  //  Fence Detected Valid when
  //  1.  Retire Valid.
  //  2.  SFence or FENCE.I instruction.
  //  3.  Has no exception.
  val fence_detected = ret_valids.head & fence_constraints_vec.head & !xpt_valid
  val fence_valid = fence_state === s_fence_req
  io.icache_sfence.valid         := (fence_detected & fence_valid) || (fence_state === s_fence_wait2)
  io.icache_sfence.bits.vpn_vld  := data_array(fence_idx).vpn_vld
  io.icache_sfence.bits.vpn      := data_array(fence_idx).vpn
  io.icache_sfence.bits.asid_vld := data_array(fence_idx).asid_vld
  io.icache_sfence.bits.asid     := data_array(fence_idx).asid
  io.dcache_sfence.valid         := (fence_detected & fence_valid) || (fence_state === s_fence_wait1)
  io.dcache_sfence.bits          := io.icache_sfence.bits

  val fence_done = state === s_fence_done

  switch (fence_state) {
    //  Step 1: Fence flush require.
    is (s_fence_idle) {
      when (fence_detected & can_retire) {
        fence_state := s_fence_req
      }
    }

    //  Step 2: Flush.
    is (s_fence_req) {
      when (io.icache_sfence.ready && io.dcache_sfence.ready) {
        fence_state := s_fence_done
      } .elsewhen (io.icache_sfence.ready) {
        fence_state := s_fence_wait1
      } .elsewhen (io.dcache_sfence.ready) {
        fence_state := s_fence_wait2
      }
    }

    //  Step 3: Wait for DCache/ICache response.
    is (s_fence_wait1) {
      when (io.dcache_sfence.ready) {
        fence_state := s_fence_req
      }
    }
    is (s_fence_wait2) {
      when (io.icache_sfence.ready) {
        fence_state := s_fence_done
      }
    }

    //  Flush Success.
    is (s_fence_done) {
      fence_state := s_fence_idle
    }
  }

  //  Update flag
  when (fence_done) {
    //  Clean FENCE flag.
    cfi_array(fence_idx).is_fence := false.B
  }

  //  Step 4: Check whether store instruction.
  //  State description:
  //  s_store_idle        : IDLE, do nothing.
  //  s_store_req         : Store Require.
  //  s_store_wait        : Wait for Response.
  //  s_store_flush       : Store need flush.
  //  s_store_done        : Execution Done.
  val s_store_idle::s_store_req::s_store_wait::s_store_flush::s_store_done::Nil = Enum(5)
  val store_state = RegInit(s_store_idle)
  val store_idx = hashIdx(io.head)

  //  Store Detected Valid when
  //  1.  Retire Valid.
  //  2.  Store instruction.
  //  3.  Has no exception.
  val store_detected = ret_valids.head & store_constraints_vec.head & !xpt_valid
  io.store_exec.req.valid       := store_state === s_store_req
  io.store_exec.req.bits.st_id  := meta_array(store_idx).st_id
  io.store_exec.req.bits.rob_id := io.head
  io.store_exec.resp.ready      := store_state === s_store_wait

  val store_flush_addr = RegNext(Mux(io.store_exec.resp.bits.cause.orR,
                          meta_array(store_idx).addr, io.store_exec.resp.bits.addr))

  val store_done = store_state === s_store_done

  switch (store_state) {
    //  Step 1: Detect store instruction.
    is (s_store_idle) {
      when (store_detected & can_retire) {
        store_state := s_store_req
      }
    }

    //  Step 2: Store execution.
    is (s_store_req) {
      when (io.store_exec.req.fire) {
        store_state := s_store_wait
      }
    }

    //  Step 3: Wait for response.
    is (s_store_wait) {
      when (io.store_exec.resp.fire) {
        store_state := Mux(io.store_exec.resp.bits.flush | io.store_exec.resp.bits.cause.orR,
                      s_store_flush, s_store_done)
      }
    }

    //  Step 4: Flush.
    is (s_store_flush) {
      store_state := s_store_done
    }

    //  Step 5: Execution Success.
    is (s_store_done) {
      store_state := s_store_idle
    }
  }

  //  Update flag
  when (io.store_exec.resp.fire) {
    cause_array(store_idx) := io.store_exec.resp.bits.cause
  }
  when (store_done) {
    //  Clean Store Flag.
    cfi_array(store_idx).is_st := false.B
  }


  //  Step 4: Check whether CSR instruction.
  //  State description:
  //  s_csr_idle        : IDLE, do nothing.
  //  s_csr_req         : CSR write Require.
  //  s_csr_done        : Execution Done.
  val s_csr_idle::s_csr_req::s_csr_done::Nil = Enum(3)
  val csr_state = RegInit(s_csr_idle)
  val csr_idx = hashIdx(io.head)
  val csr_done = csr_state === s_csr_done

  //  CSR Detected Valid when
  //  1.  Retire Valid.
  //  2.  CSR instruction.
  //  3.  Has no exception.
  val csr_detected = ret_valids.head & csr_constraints_vec.head & !xpt_valid
  io.write_port.valid  := csr_state === s_csr_req
  io.write_port.addr   := meta_array(csr_idx).csr_addr
  io.write_port.data   := data_array(csr_idx).data

  switch (csr_state) {
    //  Step 1: Detect store instruction.
    is (s_csr_idle) {
      when (csr_detected & can_retire) {
        csr_state := s_csr_req
      }
    }

    //  Step 2: Store execution.
    is (s_csr_req) {
      csr_state := s_csr_done
    }

    //  Step 3: Execution Success.
    is (s_csr_done) {
      csr_state := s_csr_idle
    }
  }

  //  Update flag
  when (csr_done) {
    cause_array(store_idx) := io.write_port.cause
    //  Clean Store Flag.
    cfi_array(store_idx).is_sys := false.B
  }

  //  Step 4: Check whether Branch
  //  State description:
  //  s_br_idle       : IDLE, do nothing.
  //  s_br_read       : BPD Read.
  //  s_br_upd        : BPD Update.
  //  s_br_wait       : Wait for response.
  //  s_br_flush      : BPD Flush (if need).
  //  s_br_done       : BPD Done.
  val s_br_idle::s_br_read::s_br_upd::s_br_wait::s_br_flush::s_br_done::Nil = Enum(6)
  val br_state = RegInit(s_br_idle)
  val br_idx = hashIdx(io.head)

  //  Branch Detect Valid when
  //  1.  Retire Valid.
  //  2.  Branch/Jump instruction.
  //  3.  Has no exception.
  val br_detected = ret_valids.head & br_constraints_vec.head & !xpt_valid

  //  BPD Predictor need Flush Valid when
  //  1.  Taken is mismatch.
  //  2.  Target address is mismatch.
  val br_need_flush = Wire(Bool()) 
  br_need_flush := (meta_array(br_idx).taken =/= data_array(br_idx).taken) ||
                   (meta_array(br_idx).tg_addr =/= data_array(br_idx).tg_addr)

  //  BPD Predictor need Update Valid when
  //  1.  Branch need Flush.
  //  2.  Branch information not match.
  val br_need_upd = Wire(Bool())
  val old_predinfo = cfi_array(br_idx)
  br_need_upd := br_need_flush |
                (old_predinfo.is_jmp  =/= cfi_array(br_idx).is_jmp  |
                 old_predinfo.is_br   =/= cfi_array(br_idx).is_br   |
                 old_predinfo.is_call =/= cfi_array(br_idx).is_call |
                 old_predinfo.is_ret  =/= cfi_array(br_idx).is_ret)
  val br_done = br_state === s_br_done

  switch (br_state) {
    //  Step 1: Detect branch/jump instruction
    is (s_br_idle) {
      when (br_detected & can_retire) {
        br_state := Mux(br_need_upd, s_br_read, s_br_done)
      }
    }

    //  Step 2: Read BPD
    is (s_br_read) {
      br_state := s_br_upd
    }

    //  Step 3: Update BPD
    is (s_br_upd) {
      br_state := Mux(br_need_flush, s_br_flush, s_br_wait)
    }


    //  Step 4: Wait for response.
    is (s_br_wait) {
      br_state := s_br_done
    }

    //  Step 4: Check whether Update.
    is (s_br_flush) {
      br_state := s_br_done
    }

    //  Step 5: Update Done.
    is (s_br_done) {
      br_state := s_br_idle
    }
  }

  //  Update Flag.
  when (br_done) {
    //  Clean Branch Flag.
    cfi_array(br_idx).is_branch := false.B
  }

  //  Update BPD
  io.bpd_upd.valid        := br_state === s_br_read
  io.bpd_upd.bits.addr    := meta_array(br_idx).addr
  io.bpd_upd.bits.len     := meta_array(br_idx).len
  io.bpd_upd.bits.taken   := data_array(br_idx).taken
  io.bpd_upd.bits.tg_addr := data_array(br_idx).tg_addr
  io.bpd_upd.bits.is_jmp  := cfi_array(br_idx).is_jmp
  io.bpd_upd.bits.is_br   := cfi_array(br_idx).is_br
  io.bpd_upd.bits.is_ret  := cfi_array(br_idx).is_ret
  io.bpd_upd.bits.is_call := cfi_array(br_idx).is_call

  //  Rob FSM
  val need_wait = xpt_valid | fence_detected | store_detected | br_detected | csr_detected

  switch (state) {
    //  Step 1: Detect whether need wait.
    is (s_normal) {
      when (need_wait & can_retire) {
        state := s_wait
      } .otherwise {
        state := s_int_chck
      }
    }

    //  Step 2: Wait for ready.
    is (s_wait) {
      when (xpt_done | fence_done | store_done | br_done | csr_done) {
        state := s_normal
      }
    }

    //  Step 3: Check interrupt.
    is (s_int_chck) {
      when (io.xpt_catch) {
        state := Mux(io.xpt_catch, s_int_flush, s_normal)
      } .otherwise {
        state := s_normal
      }
    }

    //  Step 4: Interrupts success.
    is (s_int_flush) {
      state := s_normal 
    }
  }

  val state_is_int_chck = (state === s_int_chck)
  io.interrupts.mtip   := state_is_int_chck & io.ext_interrupts.mtip
  io.interrupts.msip   := state_is_int_chck & io.ext_interrupts.msip
  io.interrupts.meip   := state_is_int_chck & io.ext_interrupts.meip
  io.interrupts.debug  := state_is_int_chck & io.ext_interrupts.debug

  val intr_flush_valid = state === s_int_flush
  val intr_flush_addr  = RegEnable(io.evec, state === s_int_chck)

  //
  val xpt_flush = xpt_state === s_xpt_flush
  val store_flush = store_state === s_store_flush
  val br_flush = br_state === s_br_flush
  io.kill.valid := xpt_flush | store_flush | br_flush | intr_flush_valid
  io.kill.bits.addr := Mux(xpt_flush, io.evec,
                          Mux(store_flush, store_flush_addr,
                            Mux(br_flush, data_array(br_idx).tg_addr, intr_flush_addr)))

  //  Read data from Rob
  for (p <- 0 until numRobReadPorts) {
    val idx = hashIdx(io.read_ports(p).addr)
    val bypass_sel = exec_reqs.map(e => e.valid & (e.rob_id === io.read_ports(p).addr))
    val bypass_vld = bypass_sel.reduce(_|_)
    val bypass_data = Mux1H(bypass_sel, exec_reqs.map(_.data))
    io.read_ports(p).valid := status_array(idx) || bypass_vld
    io.read_ports(p).data := Mux(bypass_vld, bypass_data, data_array(idx).data)
  }

  //  Performance
  val ret_metas = ret_idxs.map(idx => meta_array(hashIdx(idx)))
  val ret_cfis = ret_idxs.map(idx => cfi_array(hashIdx(idx)))
  io.perf.jump   := ret_valids.head & ret_cfis.head.is_jmp
  io.perf.br     := ret_valids.head & ret_cfis.head.is_br
  io.perf.csr    := ret_valids.head && ret_cfis.head.is_csr
  io.perf.mul    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_mul }
  io.perf.div    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_div }
  io.perf.fpu    := ret_valids zip ret_cfis map { case (v, i) => v & i.is_fpu }
  io.perf.ecall  := ret_valids.head & (ret_metas.head.uopc === UOP_ECALL)
  io.perf.ebreak := ret_valids.head & (ret_metas.head.uopc === UOP_EBREAK)
  io.perf.mispred:= br_state === s_br_flush
  io.sync        := ret_valids zip ret_cfis map { case (v, i) => v & i.is_sync } reduce (_|_)

  //  Difftest
  //  Trap
  if (env.EnableDifftest) {
    //  Commit
    for (w <- 0 until plWidth) {
      val idx = hashIdx(ret_idxs(w))
      val meta = meta_array(idx)
      val cfi = cfi_array(idx)

      val difftest = Module(new DifftestInstrCommit) 
      difftest.io.clock   := clock 
      difftest.io.coreid  := io.hart_id
      difftest.io.index   := w.U

      difftest.io.valid := RegNext(io.rets.valid && io.rets.bits(w).valid)
      difftest.io.pc    := RegNext(meta.addr)
      difftest.io.instr := DontCare
      difftest.io.special  := RegNext(cfi.is_fence |
                                      cfi.is_call |
                                      cfi.is_ret)
      difftest.io.skip  := DontCare 
      difftest.io.isRVC := DontCare
      difftest.io.rfwen := RegNext(io.rets.valid && 
                                  io.rets.bits(w).valid && 
                                  meta.ldst_vld && 
                                  meta.ldst_type === RT_FIX)
      difftest.io.fpwen := RegNext(io.rets.valid && 
                                  io.rets.bits(w).valid && 
                                  meta.ldst_vld && 
                                  meta.ldst_type === RT_FP)
      difftest.io.wpdest := DontCare
      difftest.io.wdest  := RegNext(meta.ldst_lreg)

    }



    //  Integer Write-Back
    for (w <- 0 until plWidth) {
      val idx = hashIdx(ret_idxs(w))
      val meta = meta_array(idx)
      val cfi = cfi_array(idx)
      val data = data_array(idx)

      val difftest = Module(new DifftestIntWriteback)
      difftest.io.clock := clock 
      difftest.io.coreid := io.hart_id
      difftest.io.valid := RegNext(io.rets.valid && 
                                  io.rets.bits(w).valid && 
                                  meta.ldst_vld && 
                                  meta.ldst_type === RT_FIX)
      difftest.io.dest  := RegNext(meta.ldst_lreg)
      difftest.io.data  := RegNext(data.data)
    }

    //  Floating-point Write-Back
    for (w <- 0 until plWidth) {
      val idx = hashIdx(ret_idxs(w))
      val meta = meta_array(idx)
      val cfi = cfi_array(idx)
      val data = data_array(idx)

      val difftest = Module(new DifftestFpWriteback)
      difftest.io.clock := clock 
      difftest.io.coreid := io.hart_id
      difftest.io.valid := RegNext(io.rets.valid && 
                                  io.rets.bits(w).valid && 
                                  meta.ldst_vld && 
                                  meta.ldst_type === RT_FP)
      difftest.io.dest  := RegNext(meta.ldst_lreg)
      difftest.io.data  := RegNext(data.data)
    }
  }
  //  End
}