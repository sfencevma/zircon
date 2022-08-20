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

class IssueWriteBack(implicit p: Parameters) extends BaseZirconBundle {
  val valid = Bool()
  val pntr  = UInt(robIdBits.W)
  val data  = UInt(xLen.W)
}

class IssueReplay(implicit p: Parameters) extends BaseZirconBundle {
  val replay    = Bool()
  val secondary = Bool()
  val grant     = Bool()
}

class IssueResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val secondary = Bool()
  val uop       = new IssueMicroOp
}

class IssueReservationStation(plWidth: Int, numIssueSlots: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val stall = Input(Vec(numIssuePorts, Bool()))

    val reqs = Flipped(Valid(Vec(plWidth, new IssueDispatchResp)))
    val resps = Output(Vec(numIssuePorts, new IssueResp))

    val writeback_reqs = Input(Vec(numIssuePorts, new IssueWriteBack))
    val replay_req = Flipped(Valid(new IssueReplay))

    //
    val issue_vec = Output(Vec(numRsvEntries, Bool()))
  })


  val age_mats = Seq.fill(numIssuePorts) { Module(new IssueAgeMatrix(plWidth, numIssueSlots)) }
  val slots = Seq.fill(numIssueSlots) { Module(new IssueSlot) }
  val issue_slots = VecInit(slots.map(_.io))

  //  Kill
  for (slot <- issue_slots) {
    slot.kill := io.kill
  }

  //  Age Matrix Require
  val age_mats_oh = io.reqs.bits.map(req => Fill(numIssuePorts, req.valid.asUInt) & req.uop.port)
  val age_mats_valid_vec = (0 until numIssuePorts).map(p => issue_slots.map(slot => slot.resp.valid && slot.resp.uop.port(p)))
  for ((mat, i) <- age_mats.zipWithIndex) {
    mat.io.flush := io.kill
    mat.io.reqs.valid := io.reqs.valid

    for (w <- 0 until plWidth) {
      mat.io.reqs.bits(w).valid := io.reqs.bits(w).valid && age_mats_oh(w)(i)
      mat.io.reqs.bits(w).rsv_id := io.reqs.bits(w).rsv_id
    }

    mat.io.valid_vec := age_mats_valid_vec(i)
  }

  //  Enqueue
  val enq_reqs = Wire(Vec(plWidth+1, Vec(numIssueSlots, new IssueSlotReq)))
  val reqs = io.reqs.bits.map(req => {
    val new_req = Wire(new IssueSlotReq)
    new_req.uop      := req.uop
    new_req.lrs1_map := req.lrs1_map
    new_req.lrs2_map := req.lrs2_map
    new_req.lrs3_map := req.lrs3_map
    new_req
  })
  val rsv_id_oh = io.reqs.bits.map(req => Fill(numIssueSlots, req.valid) & UIntToOH(req.rsv_id))

  for (i <- 0 until numIssueSlots)  {
    val enq_reqs_row = (rsv_id_oh.map(req => req(i)) zip reqs)
      .scanLeft(0.U.asTypeOf(new IssueSlotReq)) { case (old_req, (req, new_req)) => Mux(req, new_req, old_req) }

    for (j <- 0 until plWidth+1) {
      enq_reqs(j)(i) := enq_reqs_row(j)
    }
  }

  val enq_req_valids = rsv_id_oh.reduce(_|_)
  for ((slot, i) <- issue_slots.zipWithIndex) {
    slot.req.valid := enq_req_valids(i) & io.reqs.valid
    slot.req.bits := enq_reqs(plWidth)(i)
  }

  for (slot <- 0 until numIssueSlots) {
    issue_slots(slot).writeback_reqs := io.writeback_reqs
    issue_slots(slot).replay_req := io.replay_req
    issue_slots(slot).stall := io.stall
  }

  //  Select Logic
  val requests = issue_slots.map(req => req.resp.valid && req.resp.ready)
  val port_issued = Array.fill(numIssuePorts){Bool()}
  for (w <- 0 until numIssuePorts) {
    port_issued(w) = false.B
  }
  val issue_resps = Wire(Vec(numIssuePorts, new IssueResp))
  //  Set default
  for (w <- 0 until numIssuePorts) {
    issue_resps(w).valid := false.B
    issue_resps(w).uop   := 0.U.asTypeOf(new MicroOp)
    issue_resps(w).secondary := false.B
  }

  val wakeup_reqs = Wire(Vec(numIssuePorts, new IssueSlotWakeup))
  for (w <- 0 until numIssuePorts) {
    wakeup_reqs(w) := 0.U.asTypeOf(new IssueSlotWakeup)
  }

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).issue_req := false.B
    var uop_issued = false.B
    io.issue_vec(i) := false.B

    for (w <- 0 until numIssuePorts) {
      val can_be_issued = !io.stall(w) && issue_slots(i).resp.uop.port(w) && age_mats(w).io.oldest_vec(i)
      when (requests(i) && !uop_issued && !port_issued(w) && can_be_issued) {
        io.issue_vec(i)           := true.B
        issue_slots(i).issue_req  := true.B
        issue_resps(w).valid      := true.B
        issue_resps(w).secondary  := issue_slots(i).resp.secondary
        issue_resps(w).uop        := issue_slots(i).resp.uop

        //  TODO: Write Back Require

        wakeup_reqs(w).valid    := true.B
        wakeup_reqs(w).is_ld    := issue_slots(i).resp.uop.is_ld
        wakeup_reqs(w).wakeup   := issue_slots(i).resp.uop.wakeup
        wakeup_reqs(w).pntr     := issue_slots(i).resp.uop.rob_id
        wakeup_reqs(w).ld_track := issue_slots(i).resp.ld_track
      }

      val port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_be_issued) | port_issued(w)
      uop_issued = (requests(i) && can_be_issued && !port_issued_yet) | uop_issued
    }
  }

  val resps = Reg(Vec(numIssuePorts, new IssueResp))
  for (w <- 0 until numIssuePorts) {
    when (io.kill) {
      resps(w)  := issue_resps(w)
      resps(w).valid := false.B
    } .otherwise {
      resps(w) := issue_resps(w)
    }
  }
  io.resps := resps

  //  Wakeup Logic
  for (i <- 0 until numIssueSlots) {
    for (w <- 0 until numIssuePorts) {
      issue_slots(i).wakeup_reqs(w) := wakeup_reqs(w)
      issue_slots(i).wakeup_reqs(w).valid := !issue_slots(i).issue_req && wakeup_reqs(w).valid
    }
  }
}