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
import zircon.rob._
import zircon.util._

class LDQEntry(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val is_amo      = Bool()
  val addr        = UInt(vaddrBits.W)
  val rob_id      = UInt(robIdBits.W)
  val cr_line     = Bool()
  val fst_done    = Bool()
  val sec_done    = Bool()
  val forward     = Bool()
  val forward_id  = UInt(robIdBits.W)
  val data        = UInt(xLen.W)
  val cause       = UInt(eLen.W)

  def ready = Mux(cr_line, fst_done & sec_done, fst_done) | cause.orR
}

class LDQReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr        = UInt(vaddrBits.W)
  val rob_id      = UInt(robIdBits.W)
  val ld_id       = UInt(ldqIdBits.W)
  val secondary   = Bool()
  val is_amo      = Bool()
  val cr_line     = Bool()
  val data        = UInt(xLen.W)
  val forward     = Bool()
  val forward_id  = UInt(robIdBits.W)
}

class LDQSnoopReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val addr  = UInt(vaddrBits.W)
  val dw    = UInt(RT_SZ.W)
}

class LDQSnoopResp(implicit p: Parameters) extends BaseZirconBundle {
  val rob_id  = UInt(robIdBits.W)
  val data    = UInt(xLen.W)
}

class LDQSnoopIO(implicit p: Parameters) extends BaseZirconBundle {
  val req = Valid(new LDQSnoopReq)
  val resp = Flipped(Valid(new LDQSnoopResp))
}

class LDQMSHRReq(implicit p: Parameters) extends BaseZirconBundle {
  val ld_id     = UInt(ldqIdBits.W)
  val secondary = Bool()
  val data      = UInt(xLen.W)
  val cause     = UInt(eLen.W)
}

class LDQ(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    //  Require
    val req = Flipped(Valid(new LDQReq))
    val resp = Output(new RobExecReq)

    //  Execute
    val exec_req = Input(UInt(ldqIdBits.W))
    val exec_resp = Output(new LDQEntry)

    //
    val store_check_req = Input(UInt(robIdBits.W))
    val store_check_resp = Output(new STQExecCheckResp)

    //  MSHR Require
    val mshr_req = Flipped(Valid(new LDQMSHRReq))

    //
    val head = Input(UInt(ldqIdBits.W))
    val tail = Input(UInt(ldqIdBits.W))
  })

  val ldq = Reg(Vec(numLdqEntries, new LDQEntry))

  //  Enter Queue
  val ldq_enq_req = WireInit(io.req.bits)
  val ldq_enq_idx = hashIdx(io.req.bits.ld_id)
  when (io.req.valid) {
    ldq(ldq_enq_idx).is_amo   := io.req.bits.is_amo
    ldq(ldq_enq_idx).addr     := io.req.bits.addr
    ldq(ldq_enq_idx).rob_id   := io.req.bits.rob_id
    ldq(ldq_enq_idx).cr_line  := io.req.bits.cr_line

    //  From DCache
    when(!io.req.bits.secondary) {
      ldq(ldq_enq_idx).fst_done := true.B
      ldq(ldq_enq_idx).data     := io.req.bits.data
    }.otherwise {
      ldq(ldq_enq_idx).sec_done := true.B
      ldq(ldq_enq_idx).data     := ldq(ldq_enq_idx).data | io.req.bits.data
    }

    ldq(ldq_enq_idx).forward    := io.req.bits.forward
    ldq(ldq_enq_idx).forward_id := io.req.bits.forward_id
    ldq(ldq_enq_idx).cause      := 0.U
  }

  //  From Memory
  val mshr_req_idx = hashIdx(io.mshr_req.bits.ld_id)
  when (io.mshr_req.valid) {
    when (!io.mshr_req.bits.secondary) {
      ldq(mshr_req_idx).fst_done := true.B
      ldq(mshr_req_idx).data := io.mshr_req.bits.data
      ldq(mshr_req_idx).cause := io.mshr_req.bits.cause
    } .otherwise {
      ldq(mshr_req_idx).sec_done := true.B
      ldq(mshr_req_idx).data := io.mshr_req.bits.data

      when (!ldq(mshr_req_idx).cause.orR) {
        ldq(mshr_req_idx).cause := io.mshr_req.bits.cause
      }
    }
  }

  //
  io.exec_resp := RegNext(ldq(hashIdx(io.exec_req)))

  //
  val ldq_mask = Reverse(Cat((0 until numLdqEntries).map(slot => hashIdx(io.head) <= slot.U && hashIdx(io.tail) > slot.U)))
  val ldq_valid_vec = Mux(io.head(ldqIdBits-1)^io.tail(ldqIdBits-1), ~ldq_mask, ldq_mask)
  val check_vec = (0 until numLdqEntries).map(idx => {
    val slot = ldq(idx)
    ldq_valid_vec(idx) &
      slot.ready &
      Mux(slot.forward, (slot.rob_id === io.store_check_req) | checkOld(io.store_check_req, slot.rob_id), true.B)
  })
  val (check_sel_entry, _) = (ldq zip check_vec).foldLeft((ldq.head, check_vec.head)) {
    case (prev, next) =>
      val (prev_slot, prev_valid) = prev
      val (next_slot, next_valid) = next
      Mux(prev_valid & next_valid,
        Mux(checkOld(prev_slot.rob_id, next_slot.rob_id), prev, next),
        Mux(prev_valid, prev, next))
  }

  io.store_check_resp.flush := RegNext(check_vec reduce (_|_))
  io.store_check_resp.addr := RegNext(check_sel_entry.addr)
}