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
package zircon.issue

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.decode.rename._

class IssueSlotReq(implicit p: Parameters) extends BaseZirconBundle {
  val uop       = new MicroOp
  val lrs1_map  = new MapTableEntry
  val lrs2_map  = new MapTableEntry
  val lrs3_map  = new MapTableEntry
}

class IssueSlotResp(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid     = Bool()
  val ready     = Bool()
  val secondary = Bool()
  val ld_track  = UInt(TRACK_SZ.W)
  val uop       = new MicroOp
}

class IssueSlotWakeup(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid    = Bool()
  val is_ld    = Bool()
  val pntr     = UInt(robIdBits.W)
  val ld_track = UInt(TRACK_SZ.W)
  val wakeup   = UInt(WAKEUP_SZ.W)
}

class IssueSlotIO(val numIssuePorts: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val kill = Input(Bool())
  val req = Flipped(Valid(new IssueSlotReq))
  val resp = Output(new IssueSlotResp)
  val writeback_reqs = Input(Vec(numIssuePorts, new IssueWriteBack))
  val wakeup_reqs = Input(Vec(numIssuePorts, new IssueSlotWakeup))
  val issue_req = Input(Bool())
  val replay_req = Input(new IssueReplay)
  val stall = Input(Vec(numIssuePorts, Bool()))
}


class IssueSlot(val numIssuePorts: Int)(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
{
  val io = IO(new IssueSlotIO(numIssuePorts))

  val alive = Wire(Bool())
  val valid = Reg(Bool())
  val issued = Reg(Bool())
  val uop = Reg(new MicroOp)
  //
  //  When an entry needs to be wakeup, three situations need to be considered: 1)
  //  when the entry is vldid, it can be wakeup commonly, 2) when the entry is
  //  newly allocated, it needs to check the new data to get the latest wakeup
  //  delay, 3) when the entry is newly allocated and write back occur, rs is
  //  ready.
  val lrs1_pntr = Mux(io.req.valid, io.req.bits.uop.lrs1(robIdBits-1,0), uop.lrs1(robIdBits-1,0))
  val lrs2_pntr = Mux(io.req.valid, io.req.bits.uop.lrs2(robIdBits-1,0), uop.lrs2(robIdBits-1,0))
  val lrs3_pntr = Mux(io.req.valid, io.req.bits.uop.lrs3(robIdBits-1,0), uop.lrs3(robIdBits-1,0))


  //  ---------------------------------------------------------
  //  Meta Info
  //
  when (io.kill) {
    uop := 0.U.asTypeOf(new MicroOp)
  } .elsewhen (io.req.valid) {
    uop   := io.req.bits.uop
  }

  //  ---------------------------------------------------------
  //  LRS1  Info
  //  LRS1 Wakeup
  val lrs1_sel_wakeup = Mux1H(io.wakeup_reqs.map(_.pntr === lrs1_pntr), io.wakeup_reqs)
  val lrs1_wakeup_vld = io.wakeup_reqs.map(req => req.valid & (req.pntr === lrs1_pntr)).reduce(_|_)
  val lrs1_sel_writeback = Mux1H(io.writeback_reqs.map(_.pntr === lrs1_pntr), io.writeback_reqs)
  val lrs1_writeback_vld = io.wakeup_reqs.map(req => req.valid && (req.pntr === lrs1_pntr)).reduce(_|_)

  val lrs1_wakeup = Reg(UInt(WAKEUP_SZ.W))
  val lrs1_ready = lrs1_wakeup(0)
  when (io.req.valid) {
    lrs1_wakeup := Mux(lrs1_wakeup_vld, lrs1_sel_wakeup.wakeup,
      Mux(lrs1_writeback_vld, 1.U, Mux(io.req.bits.lrs1_map.busy, 0.U, 1.U)))
  } .elsewhen (valid && lrs1_wakeup_vld) {
    lrs1_wakeup := lrs1_sel_wakeup.wakeup
  } .elsewhen (valid && !lrs1_ready && (lrs1_wakeup =/= 0.U)) {
    lrs1_wakeup := lrs1_wakeup - 1.U
  }

  val lrs1_type = Reg(UInt(RT_SZ.W))
  when (io.req.valid) {
    lrs1_type := io.req.bits.uop.lrs1_type
  }
  when (io.req.valid) {
    uop.lrs1 := Mux(lrs1_writeback_vld, lrs1_sel_writeback.data, io.req.bits.uop.lrs1)
  } .elsewhen (valid && lrs1_writeback_vld) {
    uop.lrs1 := lrs1_sel_writeback.data
  }

  //  ---------------------------------------------------------
  //  LRS2  Info
  //  LRS2 Wakeup
  val lrs2_sel_wakeup = Mux1H(io.wakeup_reqs.map(_.pntr === lrs2_pntr), io.wakeup_reqs)
  val lrs2_wakeup_vld = io.wakeup_reqs.map(req => req.valid & (req.pntr === lrs2_pntr)).reduce(_|_)
  val lrs2_sel_writeback = Mux1H(io.writeback_reqs.map(_.pntr === lrs2_pntr), io.writeback_reqs)
  val lrs2_writeback_vld = io.wakeup_reqs.map(req => req.valid && (req.pntr === lrs2_pntr)).reduce(_|_)

  val lrs2_wakeup = Reg(UInt(WAKEUP_SZ.W))
  val lrs2_ready = lrs2_wakeup(0)
  when (io.req.valid) {
    lrs2_wakeup := Mux(lrs2_wakeup_vld, lrs2_sel_wakeup.wakeup,
      Mux(lrs2_writeback_vld, 1.U, Mux(io.req.bits.lrs2_map.busy, 0.U, 1.U)))
  } .elsewhen (valid && lrs2_wakeup_vld) {
    lrs2_wakeup := lrs2_sel_wakeup.wakeup
  } .elsewhen (valid && !lrs2_ready && (lrs2_wakeup =/= 0.U)) {
    lrs2_wakeup := lrs2_wakeup - 1.U
  }

  val lrs2_type = Reg(UInt(RT_SZ.W))
  when (io.req.valid) {
    lrs2_type := io.req.bits.uop.lrs2_type
  }
  when (io.req.valid) {
    uop.lrs2 := Mux(lrs2_writeback_vld, lrs2_sel_writeback.data, io.req.bits.uop.lrs2)
  } .elsewhen (valid && lrs2_writeback_vld) {
    uop.lrs2 := lrs2_sel_writeback.data
  }

  //  ---------------------------------------------------------
  //  LRS3  Info
  //  LRS3 Wakeup
  val lrs3_sel_wakeup = Mux1H(io.wakeup_reqs.map(_.pntr === lrs3_pntr), io.wakeup_reqs)
  val lrs3_wakeup_vld = io.wakeup_reqs.map(req => req.valid & (req.pntr === lrs3_pntr)).reduce(_|_)
  val lrs3_sel_writeback = Mux1H(io.writeback_reqs.map(_.pntr === lrs3_pntr), io.writeback_reqs)
  val lrs3_writeback_vld = io.wakeup_reqs.map(req => req.valid && (req.pntr === lrs3_pntr)).reduce(_|_)

  val lrs3_wakeup = Reg(UInt(WAKEUP_SZ.W))
  val lrs3_ready = lrs3_wakeup(0)
  when (io.req.valid) {
    lrs3_wakeup := Mux(lrs3_wakeup_vld, lrs3_sel_wakeup.wakeup,
      Mux(lrs3_writeback_vld, 1.U, Mux(io.req.bits.lrs3_map.busy, 0.U, 1.U)))
  } .elsewhen (valid && lrs3_wakeup_vld) {
    lrs3_wakeup := lrs3_sel_wakeup.wakeup
  } .elsewhen (valid && !lrs3_ready && (lrs3_wakeup =/= 0.U)) {
    lrs3_wakeup := lrs3_wakeup - 1.U
  }

  val lrs3_type = Reg(UInt(RT_SZ.W))
  when (io.req.valid) {
    lrs3_type := io.req.bits.uop.lrs3_type
  }
  when (io.req.valid) {
    uop.lrs3 := Mux(lrs3_writeback_vld, lrs3_sel_writeback.data, io.req.bits.uop.lrs3)
  } .elsewhen (valid && lrs3_writeback_vld) {
    uop.lrs3 := lrs3_sel_writeback.data
  }

  //  Issue Status
  when (io.kill) {
    issued := false.B
  } .elsewhen (io.req.valid) {
    issued := false.B
  }


  val uop_ld_track = Reg(UInt(TRACK_SZ.W))
  val wakeup_vld = (lrs1_wakeup_vld | lrs2_wakeup_vld | lrs3_wakeup_vld)
  val wakeup_has_ld = lrs1_sel_wakeup.is_ld | lrs2_sel_wakeup.is_ld | lrs3_sel_wakeup.is_ld
  when (io.kill) {
    uop_ld_track := 0.U
  } .elsewhen (io.req.valid) {
    uop_ld_track := Mux(wakeup_vld, (Fill(TRACK_SZ, wakeup_has_ld) & 4.U) | ((lrs1_sel_wakeup.ld_track | lrs2_sel_wakeup.ld_track | lrs3_sel_wakeup.ld_track) >> 1.U), 0.U)
  } .elsewhen (valid && wakeup_vld) {
    uop_ld_track := (Fill(TRACK_SZ, wakeup_has_ld) & 4.U) | (((lrs1_sel_wakeup.ld_track | lrs2_sel_wakeup.ld_track | lrs3_sel_wakeup.ld_track) >> 1.U))
  } .otherwise {
    uop_ld_track := uop_ld_track >> 1.U
  }

  //  RSV Entry Living Valid when
  //  1.  Entry Valid.
  //  2.  Not Issued Yet.
  //  3.  Not LONGPIPE OP.
  val longpipe_stall = (0 until numIssuePorts).map(p => uop.port(p) & io.stall(p)).reduce(_|_)
  alive := (valid && !issued && !io.kill && !longpipe_stall)

  //  Load Condition Valid when
  //  1.  LRS1 Track Valid.
  //  2.  LRS2 Track Valid.
  //  3.  LRS3 Track Valid.
  val load_condi = uop_ld_track.andR

  //  RSV Entry Valid when
  //  1.  Living
  //  2.  RS1 Ready
  //  3.  RS2 Ready
  //  4.  RS3 Ready
  val ready = (alive && lrs1_ready && lrs2_ready && lrs3_ready)

  //  Slot Valid
  when (io.kill) {
    valid := false.B
  } .elsewhen (io.req.valid) {
    valid := true.B
  } .elsewhen (io.issue_req) {
    when (uop.is_ld | load_condi) {
      valid := true.B
    } .otherwise {
      valid := false.B
    }
  } .elsewhen (io.replay_req.grant && !issued) {
    valid := false.B
  }

  //  Slot Issued
  val need_replay = uop_ld_track(0) && io.replay_req.replay

  when (io.kill) {
    issued := false.B
  } .elsewhen (io.req.valid) {
    issued := false.B
  } .elsewhen (io.issue_req) {
    issued := true.B
  } .elsewhen (need_replay) {
    issued := false.B
  }

  //  Replay
  val secondary = Reg(Bool())
  when (io.kill) {
    secondary := false.B
  } .elsewhen (io.req.valid) {
    secondary := false.B
  } .elsewhen (io.replay_req.replay) {
    secondary := io.replay_req.secondary
  }

  io.resp.valid := valid
  io.resp.ready := ready
  io.resp.secondary := secondary
  io.resp.uop := uop
  io.resp.ld_track := uop_ld_track
}