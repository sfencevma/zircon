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
import zircon.mmu._
import zircon.axi4._
import zircon.utils._

class AddressDecoder(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val addr = Input(UInt(vaddrBits.W))
    val mmio = Output(Bool())
  })

  val plic_offset = io.addr - PLICConstants.plicBase.U
  val plic = (plic_offset >= 0.U && plic_offset <= 0x3fffffc.U)
  val valid_clint_regs = Seq(
    ClintConstants.msip.U,
    ClintConstants.timecmp_lo.U,
    ClintConstants.timecmp_hi.U,
    ClintConstants.time_lo.U,
    ClintConstants.time_hi.U
  )
  val clint = isOneOf(io.addr,  valid_clint_regs)
  io.mmio := plic || clint
}

class LSUPerfEvents(implicit p: Parameters) extends BaseZirconBundle {
  val dtlb_access         = Bool()
  val dtlb_miss           = Bool()
  val dcache_read_access  = Bool()
  val dcache_write_access = Bool()
  val dcache_atom_access  = Bool()
  val dcache_read_miss    = Bool()
  val dcache_write_miss   = Bool()
  val dcache_atom_miss    = Bool()
}

class LSUMMIO(implicit p: Parameters) extends BaseZirconBundle {
  val req = Decoupled(new MMIOReq)
  val resp = Flipped(Decoupled(new MMIOResp))
}

class LSUIO(implicit p: Parameters) extends BaseZirconBundle {
  //  Hart ID
  val hart_id = Input(UInt(64.W))

  val kill = Input(Bool())
  val sfence = Flipped(Decoupled(new SFenceReq))
  val ptbr = Input(new PTBR)
  val prv = Input(UInt(2.W))

  //  Require
  val req = Input(new IssueResp)
  val resp = Output(new RobExecReq)

  //  Write Back
  val writeback_req = Output(new IssueWriteBack)
  val replay = Valid(new IssueReplay)

  //  Store Execution
  val store_exec = Flipped(new RobStoreExecIO)

  //  Memory
  val ptw = new DTLBPTW
  val mem = new DCacheMem
  val mmio = new LSUMMIO

  //  Performance
  val perf = Output(new LSUPerfEvents)

  //  Pointer
  val ldq_head = Input(UInt(ldqIdBits.W))
  val ldq_tail = Input(UInt(ldqIdBits.W))
  val stq_head = Input(UInt(stqIdBits.W))
  val stq_tail = Input(UInt(stqIdBits.W))

  val stall    = Output(Bool())
}

class LSU(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
  with MemoryOpConstants
{
  val io = IO(new LSUIO)

  //  LSU FSM
  //  State description.
  //  s_ready           : Ready for Memory or Memory-mapped operation.
  //  s_read_dcache     : Read DCache.
  //  s_write_dcache    : Write DCache.
  //  s_sync_write      : Write through for first page.
  //  s_store_mmio      : Write mmio.
  //  s_sync_wait       : Wait for memory response.
  //  s_mmio_read_wait  : Wait for mmio read response.
  //  s_mmio_store_wait : Wait for mmio store response.
  //  s_mmio_read       : Read mmio
  //  s_replay          : Reread DCache.
  //  s_done            : Operation done.
  val s_ready::s_read_dcache::s_write_dcache::s_sync_write::s_store_mmio::s_sync_wait:: s_mmio_read_wait::s_mmio_store_wait::s_read_mmio::s_replay::s_done::Nil = Enum(11)
  val state = RegInit(s_ready)


  //  Load Queue
  val ldq = Module(new LDQ)
  //  Store Queue
  val stq = Module(new STQ)
  //  DCache
  val dcache = Module(new DCache)

  //
  ldq.io.head := io.ldq_head
  ldq.io.tail := io.ldq_tail
  stq.io.head := io.stq_head
  stq.io.tail := io.stq_tail

  dcache.io.hart_id := io.hart_id
  dcache.io.kill    := io.kill
  dcache.io.stall   := false.B
  dcache.io.ptbr    := io.ptbr
  dcache.io.prv     := io.prv
  dcache.io.sfence  <> io.sfence
  io.ptw            <> dcache.io.ptw

  //  Performance
  io.perf.dtlb_access        := dcache.io.dtlb_access
  io.perf.dtlb_miss          := dcache.io.dtlb_miss
  io.perf.dcache_read_access := dcache.io.dcache_access
  io.perf.dcache_read_miss   := dcache.io.dcache_miss
  io.perf.dcache_atom_access := false.B
  io.perf.dcache_atom_miss   := false.B
  io.perf.dcache_write_access:= dcache.io.write.valid
  io.perf.dcache_write_miss  := dcache.io.write.valid && dcache.io.write.bits.miss

  //  LSU Micro Op
  class LSUMicroOp extends Bundle {
    val is_amo      = Bool()
    val is_mmio     = Bool()
    val is_ld       = Bool()
    val ld_id       = UInt(ldqIdBits.W)
    val is_st       = Bool()
    val st_id       = UInt(stqIdBits.W)
    val rob_id      = UInt(robIdBits.W)
    val cr_line     = Bool()
    val secondary   = Bool()
    val data_width  = UInt(DW_SZ.W)

    val ldst_vld    = Bool()
    val usign       = Bool()
    val data        = UInt(xLen.W)
    val addr        = UInt(vaddrBits.W)
  }

  //  *************************************************
  //  Stage 1: Generate PC
  def offBits: Int = log2Ceil(dcacheParams.blockBytes)

  val s1_req = WireInit(io.req)
  val lsu_addr = io.req.uop.lrs1 + io.req.uop.lrs2
  val next_line_addr = lsu_addr + 64.U
  val s1_addr = Mux(io.req.secondary, Cat(next_line_addr(vaddrBits-1, offBits), 0.U), lsu_addr)
  val dw = io.req.uop.dw
  val s1_cr_page = (s1_addr(pgIdxBits-1, 0) === 0xfff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
                   (s1_addr(pgIdxBits-1, 0) === 0xffe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
                   (s1_addr(pgIdxBits-1, 0) === 0xffd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
                   (dw === DW_64 && isOneOf(s1_addr(pgIdxBits-1,0), (0 until 4).map(i => (0xffc - i).U)))
  val s1_cr_line = (s1_addr(offBits-1, 0) === 0xff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
                   (s1_addr(offBits-1, 0) === 0xfe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
                   (s1_addr(offBits-1, 0) === 0xfd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
                   (dw === DW_64 && isOneOf(s1_addr(offBits-1,0), (0 until 4).map(i => (0xfc - i).U)))

  //  Check Memory-mapping Register Range
  val addrDecoder = Module(new AddressDecoder)
  addrDecoder.io.addr := s1_addr

  def makeUOP(req: IssueResp, mmio: Bool, cr_line: Bool, addr: UInt) = {
    val uop = Wire(new LSUMicroOp)
    uop.is_amo     := req.uop.is_amo
    uop.is_mmio    := mmio
    uop.is_ld      := req.uop.is_ld
    uop.ld_id      := req.uop.ld_id
    uop.is_st      := req.uop.is_st
    uop.st_id      := req.uop.st_id
    uop.rob_id     := req.uop.rob_id
    uop.cr_line    := cr_line
    uop.secondary  := req.secondary
    uop.data_width := req.uop.dw
    uop.ldst_vld   := req.uop.ldst_vld
    uop.usign      := req.uop.usign
    uop.data       := req.uop.lrs2
    uop.addr       := addr

    uop
  }

  //  Stage 1 Valid when
  //  1.  Require Valid.
  //  2.  Store State is READY.
  //  3.  DCache is Not BUSY.
  //  4.  Not Flush.
  val s1_vld = io.req.valid &
              (state === s_ready) &
              !dcache.io.busy &
              !(io.kill | io.sfence.valid)
  val s1_uop = makeUOP(s1_req, addrDecoder.io.mmio, s1_cr_page | s1_cr_line, s1_addr)

  //  Read Data from STQ
  stq.io.snoop.req.valid := s1_vld
  stq.io.snoop.req.bits.addr := s1_uop.addr
  stq.io.snoop.req.bits.dw := s1_uop.data_width

  //  *************************************************
  //  Stage 2: Read DCache
  val s2_vld = RegNext(s1_vld)
  val s2_uop = RegEnable(s1_uop, s1_vld)

  val replay_addr = Reg(UInt(vaddrBits.W))
  val replay_vld = Reg(Bool())
  //  Read Data from DCache Valid when
  //  1.  Stage 2 Valid.
  //  2.  It's not MMIO.
  //  3.  It's Load instruction.
  //  4.  Snoop Not Hit.
  //  5.  Replay.
  dcache.io.req.valid := (s2_vld &
                         !s2_uop.is_mmio &
                         s2_uop.is_ld &
                         !stq.io.snoop.resp.valid) |
                         (state === s_replay && replay_vld)
  dcache.io.req.bits.addr := Mux(state === s_replay, replay_addr, s2_uop.addr)

  //  *************************************************
  //  Stage 3: Select Data
  //  Stage 3 Valid when
  //  1.  Stage 2 Valid.
  //  2.  State is s_ready.
  val s3_vld = RegNext(s2_vld & state === s_ready)
  val s3_uop = RegEnable(s2_uop, s2_vld & state === s_ready)
  val snoop_data = RegEnable(stq.io.snoop.resp, s2_vld === s_ready)

  //  Load Data from DCache Valid when
  //  1.  DTLB Tag Hit.
  //  2.  DCache Tag Hit.
  val dcache_offset = s2_uop.addr(offBits-1, 0)
  val dcache_ready = !dcache.io.resp.bits.dtlb_miss & !dcache.io.resp.bits.dcache_miss

  //  Zero extend
  //  +---------+ +-------------------------------+
  //  | Extend  | |         Data                  |
  //  +---------+ +-------------------------------+
  //  | 0000000 | |          XXXXXXX              |
  //  +---------+ +-------------------------------+
  //  | 7 * 8   | |         blockBits             |
  //  +---------+ +-------------------------------+
  //
  val dcache_extend_data = Cat(0.U(56.W), dcache.io.resp.bits.dcache_data)
  val dcache_sel_datas = VecInit((0 until dcacheParams.blockBits / xLen).map(i => dcache_extend_data(xLen * (i+1)-1, xLen * i)))
  val dcache_sel_data = dcache_sel_datas(dcache_offset)

  val load_sel_data = Mux(snoop_data.valid, snoop_data.bits.data, dcache_sel_data)
  val signed_ext = !s3_uop.usign
  val load_data = MuxCase(load_sel_data,
    Array(
      (s3_uop.data_width === DW_8 ) -> bitsExtend(load_sel_data, xLen,  7, signed_ext),
      (s3_uop.data_width === DW_16) -> bitsExtend(load_sel_data, xLen, 15, signed_ext),
      (s3_uop.data_width === DW_32) -> bitsExtend(load_sel_data, xLen, 31, signed_ext)
    ))

  //  Write Data to LDQ Valid when
  //  1.  Stage 3 Valid.
  //  2.  Load instruction.
  //  3.  It's not MMIO.
  ldq.io.req.valid            := s3_vld & s3_uop.is_ld & !s3_uop.is_mmio
  ldq.io.req.bits.rob_id      := s3_uop.rob_id
  ldq.io.req.bits.ld_id       := s3_uop.ld_id
  ldq.io.req.bits.is_amo      := s3_uop.is_amo
  ldq.io.req.bits.secondary   := s3_uop.secondary
  ldq.io.req.bits.dw          := s3_uop.data_width
  ldq.io.req.bits.addr        := s3_uop.addr
  ldq.io.req.bits.cr_line     := s3_uop.cr_line
  ldq.io.req.bits.data        := load_data
  ldq.io.req.bits.forward     := snoop_data.valid
  ldq.io.req.bits.forward_id  := snoop_data.bits.rob_id

  //  Write Data to STQ Valid when
  //  1.  Stage 3 Valid.
  //  2.  Store instruction.
  stq.io.req.valid            := s3_vld & s3_uop.is_st
  stq.io.req.bits.rob_id      := s3_uop.rob_id
  stq.io.req.bits.st_id       := s3_uop.st_id
  stq.io.req.bits.is_amo      := s3_uop.is_amo
  stq.io.req.bits.is_mmio     := s3_uop.is_mmio
  stq.io.req.bits.secondary   := s3_uop.secondary
  stq.io.req.bits.addr        := s3_uop.addr
  stq.io.req.bits.data        := s3_uop.data
  stq.io.req.bits.dw          := s3_uop.data_width
  stq.io.req.bits.cr_line     := s3_uop.cr_line
  stq.io.req.bits.ppn         := dcache.io.resp.bits.dtlb_data

  //  Load Ready Valid when
  //  1.  Snoop miss.
  //  2.  DTLB Hit and DCache Hit.
  //  3.  Not cross line or secondary.
  //  4.  Has causes.
  val load_ready = (!snoop_data.valid
                || (!dcache.io.resp.bits.dtlb_miss && !dcache.io.resp.bits.dcache_miss)) &&
                (!s3_uop.cr_line || s3_uop.secondary) ||
                dcache.io.resp.bits.cause.orR

  //  Load Replay Valid when
  //  1.  Load instruction.
  //  2.  Load not ready.
  val s3_load_replay = s3_uop.is_ld & !load_ready

  //  Load Grant Valid when
  //  1.  Load ready.
  //  2.  Load instruction.
  val s3_load_grant = s3_uop.is_ld & load_ready

  //  Store Ready Valid when
  //  1.  DTLB miss.
  val store_ready = !dcache.io.resp.bits.dtlb_miss

  //  Store Replay Valid when
  //  1.  Store instruction.
  //  2.  Store Not ready.
  val s3_store_replay = s3_uop.is_st & !store_ready

  //  Store Grant Valid when
  //  1.  Store instruction.
  //  2.  Store ready.
  //  3.  Not cross line or secondary.
  //  4.  Has causes.
  val s3_store_grant = s3_uop.is_st &
                        ((store_ready & (!s3_uop.cr_line || s3_uop.secondary)) ||
                          dcache.io.resp.bits.cause.orR)

  val s3_need_replay = s3_load_replay | s3_store_replay
  val s3_need_grant = s3_load_grant | s3_store_grant
  s3_uop.secondary := Mux((s3_uop.is_st & store_ready) | (s3_uop.is_ld & load_ready),
                        Mux(s3_uop.cr_line, true.B, false.B), s3_uop.secondary)

  val s3_cause = Reg(UInt(eLen.W))

  //  Process Store Write
  val store_addr = Reg(UInt(paddrBits.W))
  val store_secondary = Reg(Bool())
  val store_data = Wire(UInt(dcacheParams.blockBits.W))
  val store_cause = Reg(UInt(eLen.W))

  //
  stq.io.exec_req := io.store_exec.req.bits.st_id
  store_data := dcache.io.resp.bits.dcache_data | (zeroExtend(stq.io.exec_resp.data, dcacheParams.blockBits) << store_addr(5, 0))  //  FIXME: Too slow ?

  switch (state) {
    //  Step 1: Load STQ entry.
    is (s_ready) {
      when (s3_uop.is_mmio & s3_uop.is_ld) {
        state := s_read_mmio 
      } .elsewhen (io.store_exec.req.fire) {
        when(ldq.io.store_check_resp.flush) {
          state := s_done
        }.otherwise {
          when(stq.io.exec_resp.is_mmio) {
            state := s_store_mmio
          }.otherwise {
            replay_vld := s3_vld
            replay_addr := s3_uop.addr
            store_secondary := false.B
            store_addr := Cat(stq.io.exec_resp.fst_ppn,
              stq.io.exec_resp.addr(pgIdxBits - 1, 0))
            state := s_read_dcache
          }
        }
      }
      s3_cause := 0.U
    }

    //  Step 2: If it's mmio write, write data by mmio.
    //  Otherwise read dcache.
    is (s_store_mmio) {
      when (io.mmio.req.fire) {
        state := s_mmio_store_wait
      }
    }
    is (s_read_mmio) {
      when (io.mmio.req.fire) {
        state := s_mmio_read_wait
      }
    }
    is (s_read_dcache) {
      when (dcache.io.resp.valid) {
        state := s_write_dcache
      }
    }

    //  Step 3: If dcache hit, combine data write to dcache and write to memary
    //  Otherwise wait for response.
    is (s_write_dcache) {
      when (!dcache.io.dcache_miss) {
        state := s_sync_write
      }
    }

    //  Step 4: Write to Memory.
    is (s_sync_write) {
      state := s_sync_wait
    }

    //  Step 5: Wait for response.
    is (s_sync_wait) {
      when (io.mem.resp.fire) {
        store_cause := io.mem.resp.bits.cause
        when (io.mem.resp.bits.cause.orR) {
          state := s_replay
        } .elsewhen (stq.io.exec_resp.cr_line && !store_secondary) {
          store_addr := Cat(stq.io.exec_resp.sec_ppn, 0.U)
          store_secondary := true.B
          state := s_read_dcache
        } .otherwise {
          state := s_replay
        }
      }
    }
    
    //  Step 6: Replay
    is (s_replay) {
      state := s_done
    }

    //  Step 7: Wait for mmio response
    is (s_mmio_read_wait) {
      when (io.mmio.resp.fire) {
        s3_cause := io.mmio.resp.bits.cause
        state := s_ready
      }
    }
    is (s_mmio_store_wait) {
      when (io.mmio.resp.fire) {
        s3_cause := io.mmio.resp.bits.cause
        state := s_ready
      }
    }

    //  Step 8: Process Done.
    is (s_done) {
      when (io.store_exec.resp.fire) {
        state := s_ready
      }
    }
    
  }

  //  Store Execution
  io.store_exec.req.ready   := state === s_ready
  io.store_exec.resp.valid  := state === s_done
  
  ldq.io.store_check_req        := io.store_exec.req.bits.rob_id
  io.store_exec.resp.bits.flush := ldq.io.store_check_resp.flush || store_cause.orR
  io.store_exec.resp.bits.cause := Mux(ldq.io.store_check_resp.flush, 0.U, store_cause) 
  io.store_exec.resp.bits.addr  := Mux(ldq.io.store_check_resp.flush, ldq.io.store_check_resp.addr, stq.io.exec_resp.addr)

  //  MMIO
  io.mmio.req.valid     := state === s_store_mmio || state === s_read_mmio
  io.mmio.req.bits.addr := Mux(state === s_read_mmio, s3_uop.addr, stq.io.exec_resp.addr)
  io.mmio.req.bits.cmd  := Mux(state === s_read_mmio, M_XRD, M_XWR)
  io.mmio.req.bits.data := stq.io.exec_resp.data

  io.mmio.resp.ready    := state === s_mmio_read_wait || state === s_mmio_store_wait

  //  Memory
  //  Require channel
  val sync_write = state === s_sync_write
  io.mem.req.valid        := sync_write || dcache.io.mem.req.valid
  io.mem.req.bits.addr    := Mux(sync_write, store_addr, dcache.io.mem.req.bits.addr)
  io.mem.req.bits.cmd     := Mux(sync_write, M_XWR, dcache.io.mem.req.bits.cmd)
  io.mem.req.bits.len     := Mux(sync_write, 4.U, dcache.io.mem.req.bits.len)
  io.mem.req.bits.size    := Mux(sync_write, memBusParams.SIZE_16B, dcache.io.mem.req.bits.size)
  io.mem.req.bits.data    := Mux(sync_write, store_data, dcache.io.mem.req.bits.data)
  dcache.io.mem.req.ready := Mux(sync_write, false.B, io.mem.req.ready) //  Sync write not need response to DCache.

  //  Response channel
  val sync_wait = state === s_sync_wait
  dcache.io.mem.resp.valid      := Mux(sync_wait, false.B,  io.mem.resp.valid)  //  Sync write not need response to DCache.
  dcache.io.mem.resp.bits.data  := io.mem.resp.bits.data
  dcache.io.mem.resp.bits.cause := io.mem.resp.bits.cause
  io.mem.resp.ready             := sync_wait || dcache.io.mem.resp.ready

  //  Write DCache
  dcache.io.write.valid        := state === s_write_dcache
  dcache.io.write.bits.miss    := dcache.io.resp.bits.dcache_miss
  dcache.io.write.bits.way_idx := dcache.io.resp.bits.way_idx
  dcache.io.write.bits.set_idx := dcache.io.resp.bits.set_idx
  dcache.io.write.bits.data    := store_data

  //  *************************************************
  //  Stage 4: Write Back
  val s4_ena = s3_vld & state === s_ready
  val s4_vld = RegNext(s4_ena)
  val s4_uop = RegEnable(s3_uop, s4_ena)
  val s4_need_replay = RegEnable(s3_need_replay, s4_ena)
  val s4_need_grant = RegEnable(s3_need_grant, s4_ena)
  val s4_data = RegEnable(ldq.io.resp.data, s3_vld & state === s_ready)
  val s4_cause = RegEnable(s3_cause, s4_ena)

  //  Write Back
  io.writeback_req.valid := s4_vld & s4_uop.ldst_vld
  io.writeback_req.pntr  := s4_uop.rob_id
  io.writeback_req.data  := s4_data

  //  Replay
  io.replay.valid           := s4_vld
  io.replay.bits.replay     := s4_need_replay
  io.replay.bits.grant      := s4_need_grant
  io.replay.bits.secondary  := s4_uop.secondary

  //  Execute done
  io.resp.valid   := s4_vld & s4_need_grant
  io.resp.rob_id  := s4_uop.rob_id
  io.resp.data    := s4_data
  io.resp.cause   := s4_cause

  //
  io.store_exec.resp.valid      := state === s_done
  io.store_exec.resp.bits.flush := store_cause.orR | ldq.io.store_check_resp.flush
  io.store_exec.resp.bits.addr  := Mux(ldq.io.store_check_resp.flush,
                                        ldq.io.store_check_resp.addr, stq.io.exec_resp.addr)
  io.store_exec.resp.bits.cause := store_cause

  io.stall := state =/= s_ready
  //  End
}