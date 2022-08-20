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
package top

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import zircon.frontend._
import zircon.exu._
import zircon.axi4._
import zircon.common._
import zircon.mmu._
import difftest._

class ZirconCore(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val hart_id = Input(UInt(64.W))
    val reset_vector = Input(UInt(vaddrBits.W))
    val interrupts = Input(new Interrupts)

    val mem_node = AXI4MasterBundle(memBusParams)
    val mmap_node = AXI4MasterBundle(mmapBusParams)
  })

  //  *************************************
  //  Frontend
  val frontend = Module(new Frontend())

  //  ************************************************
  //  dec0
  val decode = Module(new Decode(decodeWidth))

  //  ************************************************
  //  dec1
  val rename = Module(new RenameStage(decodeWidth))
  val allocator = Module(new Allocator(decodeWidth))

  //  ************************************************
  //  iss0
  val issue_read = Module(new IssueRead(decodeWidth, 3 * decodeWidth))
  val issue_dispatch = Module(new IssueDispatch(decodeWidth, numIssuePorts))
  val issue_rsv = Module(new IssueReservationStation(decodeWidth, numRsvEntries))
  val iregfiles = Module(new RegFile(numLRegs, 3 * decodeWidth, retireWidth, xLen, false))
  val fregfiles = Module(new RegFile(numLRegs, 3 * decodeWidth, retireWidth, xLen, true))

  //  ************************************************
  //  exec
  val alu = Module(new ALU)
  val bru = Module(new BRU)
  val mul = Module(new MUL)
  val div = Module(new DIV)
  val lsu = Module(new LSU)
  val csr = Module(new CustomCSRs)

  //  ************************************************
  //  Rob
  val rob = Module(new ROB(decodeWidth, numIssuePorts, 3 * decodeWidth))

  //  ************************************************
  //  Rob
  val mmu = Module(new MMU())

  //  Global signal
  val global_kill = rob.io.kill.valid

  //  ************************************************
  //  Backend to Frontend.
  frontend.io.hart_id := io.hart_id
  frontend.io.reset_vector := io.reset_vector
  frontend.io.kill <> rob.io.kill
  frontend.io.stall := decode.io.stall
  frontend.io.ptbr := csr.io.ptbr
  frontend.io.prv := csr.io.prv
  frontend.io.sfence <> rob.io.icache_sfence
  frontend.io.sync := rob.io.sync
  frontend.io.bpd_upd <> rob.io.bpd_upd

  //  Memory
  mmu.io.itlb_ptw <> frontend.io.ptw
  mmu.io.icache_mem <> frontend.io.mem

  //  Performance
  csr.io.perf.itlb_access := frontend.io.perf.itlb_access
  csr.io.perf.itlb_miss := frontend.io.perf.itlb_miss
  csr.io.perf.icache_access := frontend.io.perf.icache_access
  csr.io.perf.icache_miss := frontend.io.perf.icache_miss

  //  ************************************************
  //  Frontend to Decode
  decode.io.kill  := global_kill
  decode.io.stall := rename.io.stall
  decode.io.reqs  <> frontend.io.resp

  //  ************************************************
  //  Decode to Rename & Allocator
  allocator.io.kill       := global_kill
  allocator.io.reqs.valid := decode.io.resps.valid
  rename.io.kill          := global_kill

  rename.io.kill := global_kill
  rename.io.stall := allocator.io.empty
  rename.io.reqs.valid := decode.io.resps.valid
  for (w <- 0 until decodeWidth) {
    val inst = decode.io.resps.bits(w)
    allocator.io.reqs.bits(w).valid := inst.valid
    allocator.io.reqs.bits(w).is_st := inst.uop.is_st
    allocator.io.reqs.bits(w).is_ld := inst.uop.is_ld

    rename.io.reqs.bits(w).valid := inst.valid
    rename.io.reqs.bits(w).uop   := inst.uop
    rename.io.reqs.bits(w).uop.rob_id := allocator.io.resps(w).rob_id
    rename.io.reqs.bits(w).uop.st_id  := allocator.io.resps(w).st_id
    rename.io.reqs.bits(w).uop.ld_id  := allocator.io.resps(w).ld_id
    rename.io.reqs.bits(w).uop.rsv_id := allocator.io.resps(w).rsv_id
    rename.io.reqs.bits(w).taken      := decode.io.resps.bits(w).taken
    rename.io.reqs.bits(w).tg_addr    := decode.io.resps.bits(w).tg_addr
  }
  allocator.io.rsv_dealloc := issue_rsv.io.issue_vec

  //  Update rename table.
  rename.io.commit_reqs.valid := rob.io.rets.valid
  for (w <- 0 until retireWidth) {
    rename.io.commit_reqs.bits(w).valid     := rob.io.rets.bits(w).valid
    rename.io.commit_reqs.bits(w).rob_id    := rob.io.rets.bits(w).rob_id
    rename.io.commit_reqs.bits(w).ldst_type := rob.io.rets.bits(w).ldst_type
    rename.io.commit_reqs.bits(w).ldst_lreg := rob.io.rets.bits(w).ldst_lreg
  }
  //  ************************************************
  //  Issue Read
  issue_read.io.kill := global_kill
  issue_read.io.reqs <> rename.io.resps

  //  Read data
  iregfiles.io.read_ports <> issue_read.io.reg_read_ports
  fregfiles.io.read_ports <> issue_read.io.fp_reg_read_ports
  csr.io.read_port <> issue_read.io.csr_read_port
  rob.io.read_ports <> issue_read.io.rob_read_ports

  //  ************************************************
  //  Issue Dispatch
  issue_dispatch.io.reqs <> issue_read.io.resps
  issue_dispatch.io.writeback_reqs := issue_rsv.io.writeback_reqs
  rob.io.reqs := issue_dispatch.io.disp

  //  ************************************************
  //  Issue Reverve Station
  //  Port 0: ALU
  //  Port 1: BRU
  //  Port 2: MUL
  //  Port 3: DIV
  //  Port 4: LSU
  issue_rsv.io.kill     := global_kill
  issue_rsv.io.stall(0) := false.B
  issue_rsv.io.stall(1) := false.B
  issue_rsv.io.stall(2) := false.B
  issue_rsv.io.stall(3) := div.io.stall
  issue_rsv.io.stall(4) := lsu.io.stall
  issue_rsv.io.reqs     := issue_dispatch.io.resps

  //  Write Back
  issue_rsv.io.writeback_reqs(0) := alu.io.writeback_req
  issue_rsv.io.writeback_reqs(1) := bru.io.writeback_req
  issue_rsv.io.writeback_reqs(2) := mul.io.writeback_req
  issue_rsv.io.writeback_reqs(3) := div.io.writeback_req
  issue_rsv.io.writeback_reqs(4) := lsu.io.writeback_req

  // issue_rsv.io.writeback_reqs(4) := fpu.io.writeback_req
  // issue_rsv.io.writeback_reqs(5) := fdiv.io.writeback_req
  issue_rsv.io.replay_req := lsu.io.replay
  issue_dispatch.io.writeback_reqs := issue_rsv.io.writeback_reqs

  //  ************************************************
  //  Execution
  alu.io.prv      := csr.io.prv
  alu.io.req      := issue_rsv.io.resps(0)
  bru.io.req      := issue_rsv.io.resps(1)
  mul.io.req      := issue_rsv.io.resps(2)
  div.io.req      := issue_rsv.io.resps(3)

  lsu.io.hart_id  := io.hart_id
  lsu.io.kill     := global_kill
  lsu.io.sfence   <> rob.io.dcache_sfence
  lsu.io.ptbr     := csr.io.ptbr
  lsu.io.prv      := csr.io.prv
  lsu.io.req      := issue_rsv.io.resps(4)
  lsu.io.ldq_head := allocator.io.ldq_head
  lsu.io.ldq_tail := allocator.io.ldq_tail
  lsu.io.stq_head := allocator.io.stq_head
  lsu.io.stq_tail := allocator.io.stq_tail

  mmu.io.dtlb_ptw <> lsu.io.ptw
  mmu.io.dcache_mem <> lsu.io.mem

  //  CSR
  //  Performance
  csr.io.perf.dtlb_access          := lsu.io.perf.dtlb_access
  csr.io.perf.dtlb_miss            := lsu.io.perf.dtlb_miss
  csr.io.perf.dcache_read_access   := lsu.io.perf.dcache_read_access
  csr.io.perf.dcache_write_access  := lsu.io.perf.dcache_write_access
  csr.io.perf.dcache_atom_access   := lsu.io.perf.dcache_atom_access
  csr.io.perf.dcache_read_miss     := lsu.io.perf.dcache_read_miss
  csr.io.perf.dcache_write_miss    := lsu.io.perf.dcache_write_miss
  csr.io.perf.dcache_atom_miss     := lsu.io.perf.dcache_atom_miss
  csr.io.perf.is_jmp               := rob.io.perf.jump
  csr.io.perf.is_br                := rob.io.perf.br
  csr.io.perf.is_csr               := rob.io.perf.csr
  csr.io.perf.is_mdus              := rob.io.perf.mul zip rob.io.perf.div map { case (m, d) => m | d }
  csr.io.perf.is_fpus              := 0.U(retireWidth.W).asBools
  csr.io.perf.is_ebreak            := rob.io.perf.ebreak
  csr.io.perf.is_ecall             := rob.io.perf.ecall
  csr.io.perf.mispred              := rob.io.perf.mispred
  csr.io.perf.stall                := DontCare
  csr.io.perf.flush                := rob.io.kill.valid
  csr.io.ret                       := rob.io.rets.bits.map(_.valid & rob.io.rets.valid)
  csr.io.interrupts                <> rob.io.interrupts
  csr.io.xpt                       := rob.io.xpt
  csr.io.hart_id                   := io.hart_id

  //  ************************************************
  //  Rob
  allocator.io.dealloc  := rob.io.rets
  rob.io.hart_id        := io.hart_id
  rob.io.stall          := csr.io.stall
  rob.io.ext_interrupts := io.interrupts
  rob.io.head           := allocator.io.rob_head
  rob.io.tail           := allocator.io.rob_tail

  rob.io.alu_req := alu.io.resp
  rob.io.bru_req := bru.io.resp
  rob.io.mul_req := mul.io.resp
  rob.io.div_req := div.io.resp
  rob.io.lsu_req := lsu.io.resp

  lsu.io.store_exec <> rob.io.store_exec
  rob.io.xpt_catch := csr.io.xpt_catch
  rob.io.evec := csr.io.evec

  //  ************************************************
  //  Write Back
  for (w <- 0 until retireWidth) {
    val ret = rob.io.rets.bits(w)
    //  Integer
    iregfiles.io.write_ports(w).valid     := rob.io.rets.valid && ret.valid && ret.ldst_vld && ret.ldst_type === RT_FIX
    iregfiles.io.write_ports(w).bits.addr := ret.ldst_lreg
    iregfiles.io.write_ports(w).bits.data := ret.ldst

    //  Floating-point
    fregfiles.io.write_ports(w).valid     := rob.io.rets.valid && ret.valid && ret.ldst_vld && ret.ldst_type === RT_FP
    fregfiles.io.write_ports(w).bits.addr := ret.ldst_lreg
    fregfiles.io.write_ports(w).bits.data := ret.ldst
  }

  iregfiles.io.hart_id := io.hart_id
  fregfiles.io.hart_id := io.hart_id

  csr.io.write_port <> rob.io.write_port
  csr.io.set_fs_dirty := false.B  //  not support fpu

  //  ************************************************
  //  MMU
  mmu.io.hart_id := io.hart_id
  mmu.io.kill    := global_kill
  mmu.io.prv     := csr.io.prv
  mmu.io.ptbr    := csr.io.ptbr
  mmu.io.tsr     := csr.io.tsr
  mmu.io.sum     := csr.io.sum
  mmu.io.mprv    := csr.io.mprv

  //  MMIO
  mmu.io.mmio <> lsu.io.mmio

  io.mem_node <> mmu.io.mem_node
  io.mmap_node <> mmu.io.mmap_node
  //  End
}