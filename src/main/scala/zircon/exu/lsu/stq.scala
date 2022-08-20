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
import zircon.utils._


class STQEntry(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val rob_id    = UInt(robIdBits.W)
  val addr      = UInt(vaddrBits.W)
  val data      = UInt(xLen.W)
  val dw        = UInt(DW_SZ.W)
  val is_amo    = Bool()
  val is_mmio   = Bool()
  val cr_line   = Bool()
  val fst_done  = Bool()
  val fst_ppn   = UInt(ppnBits.W)
  val sec_done  = Bool()
  val sec_ppn   = UInt(ppnBits.W)

  def ready = Mux(cr_line, fst_done & sec_done, fst_done)
}

class STQReq(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val rob_id    = UInt(robIdBits.W)
  val st_id     = UInt(stqIdBits.W)
  val secondary = Bool()
  val is_amo    = Bool()
  val is_mmio   = Bool()
  val addr      = UInt(vaddrBits.W)
  val data      = UInt(xLen.W)
  val dw        = UInt(DW_SZ.W)
  val cr_line   = Bool()
  val ppn       = UInt(ppnBits.W)
}

class STQExecCheckResp(implicit p: Parameters) extends BaseZirconBundle {
  val flush = Bool()
  val addr  = UInt(vaddrBits.W)
}

class STQ(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    //  Require
    val req = Flipped(Valid(new STQReq))

    //  Execute Require
    val exec_req = Input(UInt(stqIdBits.W))
    val exec_resp = Output(new STQEntry)

    //  LDQ Snoop
    val snoop = Flipped(new LDQSnoopIO)

    //
    val head = Input(UInt(stqIdBits.W))
    val tail = Input(UInt(stqIdBits.W))
  })

  val stq = Reg(Vec(numStqEntries, new STQEntry))

  //  Enter Queue
  val stq_enq_req = WireInit(io.req.bits)
  val stq_enq_idx = hashIdx(io.req.bits.st_id)
  val full_mask = MuxCase(~0.U(xLen.W),
    Array(
      (stq_enq_req.dw === DW_8 ) -> 0xff.U,
      (stq_enq_req.dw === DW_16) -> 0xffff.U,
      (stq_enq_req.dw === DW_32) -> Fill(32, 1.U)
    ))

  when (io.req.valid) {
    stq(stq_enq_idx).is_amo := stq_enq_req.is_amo
    stq(stq_enq_idx).is_mmio:= stq_enq_req.is_mmio
    stq(stq_enq_idx).rob_id := stq_enq_req.rob_id
    stq(stq_enq_idx).addr   := stq_enq_req.addr
    stq(stq_enq_idx).data   := MaskData(0.U, stq_enq_req.data, full_mask)
    stq(stq_enq_idx).dw     := stq_enq_req.dw
    stq(stq_enq_idx).cr_line := stq_enq_req.cr_line

    when(!io.req.bits.secondary) {
      stq(stq_enq_idx).fst_done := true.B
      stq(stq_enq_idx).fst_ppn  := stq_enq_req.ppn
    }.otherwise {
      stq(stq_enq_idx).sec_done := true.B
      stq(stq_enq_idx).sec_ppn  := stq_enq_req.ppn
    }
  }

  //  LDQ Snoop
  val stq_mask = Reverse(Cat((0 until numStqEntries).map(slot => hashIdx(io.head) <= slot.U && hashIdx(io.tail) > slot.U)))
  val stq_valid_vec = Mux(io.head(stqIdBits-1)^io.tail(stqIdBits-1), ~stq_mask, stq_mask)
  val snoop_hit = stq.map(slot => slot.addr === io.snoop.req.bits.addr && slot.dw >= io.snoop.req.bits.dw) reduce (_|_)
  val (snoop_sel_entry, _) = (stq zip stq_valid_vec.asBools).foldLeft((stq.head, stq_valid_vec.asBools.head)) {
    case (prev, next) =>
      val (prev_slot, prev_valid) = prev
      val (next_slot, next_valid) = next
      val slot = Mux(prev_valid & next_valid,
                  Mux(checkOld(prev_slot.rob_id, next_slot.rob_id), next_slot, prev_slot),
                    Mux(prev_valid, prev_slot, next_slot))
      val valid = Mux(prev_valid & next_valid,
                    Mux(checkOld(prev_slot.rob_id, next_slot.rob_id), next_valid, prev_valid),
                     Mux(prev_valid, prev_valid, next_valid))
      (slot, valid)
  }

  io.snoop.resp.valid := RegNext(io.snoop.req.valid & snoop_hit)
  io.snoop.resp.bits.rob_id := RegNext(snoop_sel_entry.rob_id)
  io.snoop.resp.bits.data := RegNext(snoop_sel_entry.data)

  //
  io.exec_resp := stq(hashIdx(io.exec_req))

  // End
}