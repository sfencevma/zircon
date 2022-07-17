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
import zircon.issue._
import zircon.exu.lsu._
import zircon.mmu._
import zircon.util._
import zircon.rob._
import zircon.axi4._

class LSUIO(implicit p: Parameters) extends BaseZirconBundle {
  val flush = Input(Bool())
  val sfence = Flipped(Decoupled(new SFenceReq))

  //  Require
  val req = Input(new IssueResp)
  val resp = Output(new RobExecReq)

  //  Write Back
  val writeback_req = Output(new IssueWriteBack)
  val replay = Valid(new IssueReplay)

  //  Write Dcache
  val store_exec_req = Flipped(new RobStoreExecIO())

  //
  val ldq_head = Input(UInt(ldqIdBits.W))
  val ldq_tail = Input(UInt(ldqIdBits.W))
  val stq_head = Input(UInt(stqIdBits.W))
  val stq_tail = Input(UInt(stqIdBits.W))

  //  Forward Stall
  val forward_stall = Output(Bool())
}

class LSU(params: Seq[AXI4MasterParams])(implicit p: Parameters) extends BaseAXI4MasterModule(params, new LSUIO)
  with ScalarOpConstants
  with HasZirconCoreParameters
{
  override lazy val module = new AXI4MasterModuleImp[LSUIO](this) {
    val ext_io = io.extend_io.get
    val (out, _) = node.out.head

    val pipeline_ena = false.B
    val s1_req = WireInit(ext_io.req)

    //  ----------------------------------------------------------------------
    //  Stage 1: Generate PC
    def offBits: Int = log2Ceil(dcacheParams.blockBits)

    val lsu_addr = ext_io.req.uop.lrs1 + ext_io.req.uop.lrs2
    val next_line_addr = lsu_addr + 64.U
    val s1_addr = Mux(ext_io.req.secondary, Cat(next_line_addr(vaddrBits-1, offBits), 0.U), lsu_addr)
    val dw = ext_io.req.uop.dw

    val s1_cr_page = (s1_addr(pgIdxBits-1, 0) === 0xfff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
      (s1_addr(pgIdxBits-1, 0) === 0xffe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
      (s1_addr(pgIdxBits-1, 0) === 0xffd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
      (dw === DW_64 && isOneOf(s1_addr(pgIdxBits-1,0), (0 until 4).map(i => (0xffc - i).U)))
    val s1_cr_line = (s1_addr(offBits-1, 0) === 0xff.asUInt && isOneOf(dw, Seq(DW_16, DW_32, DW_64))) ||
      (s1_addr(offBits-1, 0) === 0xfe.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
      (s1_addr(offBits-1, 0) === 0xfd.asUInt && isOneOf(dw, Seq(DW_32, DW_64))) ||
      (dw === DW_64 && isOneOf(s1_addr(offBits-1,0), (0 until 4).map(i => (0xfc - i).U)))

    //  Check Memory-mapping Register Range


    //  ----------------------------------------------------------------------
    //  Stage 2: Read DCache
    val ldq = Module(new LDQ)
    val stq = Module(new STQ)
    val dtlb = Module(new DTLB(dcacheParams.nSets, dcacheParams.nWays))
    val dcache = Module(new DCache())

    val s2_req = RegEnable(s1_req, pipeline_ena)
    val s2_cr_line = RegEnable(s1_cr_line | s1_cr_page, pipeline_ena)
    //  Step1 : Read Data from LDQ, STQ, DTLB, DCache (if need)
    //  Read from LDQ
    ldq.io.exec_req := s2_req.uop.ld_id

    //  Read from STQ
    stq.io.snoop.req.valid := s2_req.valid & s2_req.uop.is_ld
    stq.io.snoop.req.bits.addr := s2_req.uop.addr
    stq.io.snoop.req.bits.dw := s2_req.uop.dw

    //  Read from DTLB
    dtlb.io.req.valid := s2_req.valid
    dtlb.io.req.bits.addr := s2_req.uop.addr

    //  Read from DCache
    dcache.io.req.valid := s2_req.valid
    dcache.io.req.bits.addr := s2_req.uop.addr

    //  Step 2: Select Data
    val uop_is_ld = s2_req.uop.is_ld
    val uop_is_st = s2_req.uop.is_st

    //  Step 2.1: If Load instruction
    //  Load Data from LDQ Valid when
    //  1.  If secondary, two part data are ready.
    //  2.  If not secondary, 1) not cross and first part done.
    //  3.  Has no exception.
    val ldq_ready = ((s2_req.secondary & ldq.io.exec_resp.fst_done & ldq.io.exec_resp.sec_done) |
                    (!s2_req.secondary &  s2_cr_line & ldq.io.exec_resp.fst_done)) &
                    !ldq.io.exec_resp.cause.orR

    //  Load Data from STQ Valid when
    //  1.  Address hit
    //  2.  Data Width contains
    val stq_ready = stq.io.snoop.resp.valid

    //  Load Data from DCache when
    //  1.  DTLB hit.
    //  2.  DCache hit.
    val dcache_ready = !dtlb.io.resp.miss & !dcache.io.resp.miss
    val dcache_sel_data = 0.U

    val load_ready = ldq_ready | stq_ready | dcache_ready
    val load_data = Mux(ldq_ready, ldq.io.resp.data, Mux(stq_ready, stq.io.snoop.resp.bits.data, dcache_sel_data))
    //  Mask Data

    //  Step 2.2: Write to LDQ
    ldq.io.req.valid            := s2_req.valid & uop_is_ld & load_ready
    ldq.io.req.bits.addr        := s2_req.uop.addr
    ldq.io.req.bits.rob_id      := s2_req.uop.rob_id
    ldq.io.req.bits.secondary   := s2_req.secondary
    ldq.io.req.bits.is_amo      := s2_req.uop.is_amo
    ldq.io.req.bits.cr_line     := s2_cr_line
    ldq.io.req.bits.data        := load_data
    ldq.io.req.bits.forward     := stq_ready & !ldq_ready
    ldq.io.req.bits.forward_id  := stq.io.snoop.resp.bits.rob_id

    //  Step 2.3: If Store instruction
    //  1.  DTLB hit
    val dtlb_ready = !dtlb.io.resp.miss

    //  Step 2.4: Write to STQ
    stq.io.req.valid          := s2_req.valid & uop_is_st & dtlb_ready
    stq.io.req.bits.addr      := s2_req.uop.addr
    stq.io.req.bits.rob_id    := s2_req.uop.rob_id
    stq.io.req.bits.st_id     := s2_req.uop.st_id
    stq.io.req.bits.secondary := s2_req.secondary
    stq.io.req.bits.is_amo    := s2_req.uop.is_amo
    stq.io.req.bits.cr_line   := s2_cr_line
    stq.io.req.bits.dw        := s2_req.uop.dw
    stq.io.req.bits.data      := s2_req.uop.lrs2
    stq.io.req.bits.ppn       := dtlb.io.resp.ppn

    //  Step 2.5: Check whether need replay.
    val load_replay = !load_ready
    val load_grant = (!s2_cr_line | s2_req.secondary) & load_ready
    val load_secondary = s2_cr_line & dcache_ready & !s2_req.secondary
    val store_replay = !dtlb_ready
    val store_grant = (!s2_cr_line | s2_req.secondary) & dtlb_ready
    val store_secondary = s2_cr_line & dtlb_ready & !s2_req.secondary

    //  --------------------------------------------------------------------
    //  Stage 3:  Write Back
    val s3_req = RegEnable(s2_req, pipeline_ena)
    val s3_data = RegEnable(load_data, pipeline_ena)
    val s3_load_replay = RegEnable(load_replay, pipeline_ena)
    val s3_load_grant = RegEnable(load_grant, pipeline_ena)
    val s3_load_secondary = RegEnable(load_secondary, pipeline_ena)
    val s3_store_replay = RegEnable(store_replay, pipeline_ena)
    val s3_store_grant = RegNext(store_grant, pipeline_ena)
    val s3_store_secondary = RegEnable(store_secondary, pipeline_ena)

    ext_io.writeback_req.valid  := s3_req.valid & s3_req.uop.ldst_vld
    ext_io.writeback_req.pntr   := s3_req.uop.rob_id
    ext_io.writeback_req.data   := s3_data

    //  Stage 3:  Replay or Grant
    ext_io.replay.valid          := s3_req.valid
    ext_io.replay.bits.replay    := Mux(s3_req.uop.is_ld, s3_load_replay, s3_store_replay) // or Load or Store MMR
    ext_io.replay.bits.grant     := Mux(s3_req.uop.is_ld, s3_load_grant, s3_store_grant)
    ext_io.replay.bits.secondary := Mux(s3_req.uop.is_ld, s3_load_secondary, s3_store_secondary)

    //  MSHR
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
    mshr.io.flush.get := ext_io.flush

    //  Step 1
    //  1.  Load. 1) If Snoop hit, go on. 2) DTLB or DCache miss, to MSHR.
    //  2.  Store. 1) If DTLB miss, to MSHR.
    val new_req = Wire(new MSHREntry)
    new_req.is_phy    := !dtlb.io.resp.miss
    new_req.addr      := Mux(dtlb.io.resp.miss, s2_req.uop.addr, Cat(dtlb.io.resp.ppn, s2_req.uop.addr(pgIdxBits-1,0)))
    new_req.is_ld     := s2_req.uop.is_ld
    new_req.ld_id     := s2_req.uop.ld_id
    new_req.is_st     := s2_req.uop.is_st
    new_req.st_id     := s2_req.uop.st_id
    new_req.secondary := s2_req.secondary

    mshr.io.enq.valid := s2_req.valid & ((uop_is_ld & s2_req.uop.is_ld) | (uop_is_st & s2_req.uop.is_st))
    mshr.io.enq.bits  := new_req
    pipeline_ena      := mshr.io.count < nMSHRs.U

    //  Store Execute FSM
    val s_ready::s_read_dcache::s_write_dcache::s_write_regs::s_sync_wait::s_done::Nil = Enum(4)
    val st_state = RegInit(s_ready)

    stq.io.exec_req := ext_io.store_exec_req.req.bits.st_id
    ldq.io.store_check_req := stq.io.exec_req
    val stq_write_ready = Mux(stq.io.exec_resp.cr_line, stq.io.exec_resp.fst_done & stq.io.exec_resp.sec_done, stq.io.exec_resp.fst_done)
    val load_fail = ldq.io.store_check_resp.flush
    val stq_write_fail = load_fail | stq.io.exec_resp.cause.orR
    val is_mmr = false.B
    switch (st_state) {
      is (s_ready) {
        when (ext_io.store_exec_req.req.valid) {
          when (stq_write_fail) {
            st_state := s_done
          } .elsewhen(is_mmr) {
            st_state := s_write_regs
          } .elsewhen (stq_write_ready) {
            st_state := s_read_dcache
          }
        }
      }
      is (s_read_dcache) {
        st_state := s_write_dcache
      }
      is (s_write_dcache) {
        st_state := s_sync_wait
      }
      is (s_write_regs) {
        st_state := s_sync_wait
      }
      is (s_sync_wait) {
        st_state := s_done
      }
      is (s_done) {
        st_state := s_ready
      }
    }

    ext_io.store_exec_req.resp.valid          := st_state === s_done
    ext_io.store_exec_req.resp.bits.flush     := RegNext(stq.io.exec_resp.cause.orR)
    ext_io.store_exec_req.resp.bits.ld_flush  := RegNext(load_fail)
    ext_io.store_exec_req.resp.bits.ld_addr   := RegNext(ldq.io.store_check_resp.addr)
    ext_io.store_exec_req.resp.bits.cause     := RegNext(stq.io.exec_resp.cause)

    //  AXI4 FSM
    val s_idle::s_write_req::s_write_data::s_write_wait::s_read_req::s_read_wait::Nil = Enum(6)
    val axi_fsm = RegInit(s_idle)


    //  Read Part
    switch (axi_fsm) {
      is (s_idle) {
        when (mshr.io.count > 0.U) {
          axi_fsm := s_read_req
        }
      }
      is (s_read_req) {
        when (out.ar.fire) {
          axi_fsm := s_read_wait
        }
      }
      is (s_read_wait) {
        when (out.r.fire) {
          axi_fsm := s_idle
        }
      }
    }

    //  Write Part
    val w_axi_fsm = RegInit(s_idle)
    switch (w_axi_fsm) {
      is (s_idle) {
        when (st_state === s_sync_wait) {
          w_axi_fsm := s_write_req
        }
      }
      is (s_write_req) {
        when (out.aw.fire) {
          w_axi_fsm := s_write_data
        }
      }
      is (s_write_data) {
        when (out.w.fire & out.w.bits.last) {
          w_axi_fsm := s_write_wait
        }
      }
      is (s_write_wait) {
        when (out.b.fire) {
          w_axi_fsm := s_idle
        }
      }
    }

    //  TODO: MMR Check, AXI4 FSM Complete and PTW FSM
  }
}