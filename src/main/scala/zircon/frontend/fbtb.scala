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

class FastBTBReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class FastBTBResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss    = Bool()
  val tg_addr = UInt(vaddrBits.W)
  val offset  = UInt(4.W)
  val br_type = Bool()
}

class FastBTBUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr      = UInt(vaddrBits.W)
  val tg_addr   = UInt(vaddrBits.W)
  val offset    = UInt(4.W)
  val br_type   = Bool()  //  0. Branch, 1. Jump
}


class FastBTB(nSets: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new FastBTBReq))
    val resp = Output(new FastBTBResp)
    val upd = Flipped(Valid(new FastBTBUpdate))
  })

  def offBits: Int = log2Ceil(icacheParams.fetchBytes)
  def tagBits: Int = vpnBits - offBits
  def predBits: Int = vaddrBits + offBits + 1
  def compute_idx(addr: UInt) = {
    val lgSets = log2Ceil(nSets)
    val base = offBits
    addr(base + lgSets - 1, base)
  }
  def compute_tag(addr: UInt) = addr(vaddrBits-1, offBits)

  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(nSets) - 1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.asUInt.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(nSets-1,1)
  }

  def get_replace_way(state: UInt) = {
    val shift_state = state << 1
    var idx = 1.U(1.W)

    for (i <- log2Up(nSets) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(nSets)-1, 0) < nSets.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(nSets)-1,0)
  }

  class FastBTBPredBundle extends Bundle {
    val tg_addr = UInt(vaddrBits.W)
    val offset  = UInt(offBits.W)
    val br_type = Bool()
  }

  //  ******************************************
  //  Fast BTB
  //  +----+-----------+---------------+
  //  | V  |    Tag    |      Pred     |
  //  +----+-----------+---------------+
  //  |               .......          |
  //  |                                |
  //  +----+-----------+---------------+
  //  | V  |    Tag    |      Pred     |
  //  +----+-----------+---------------+
  //  | 1  |   tagBits |      predBits |
  //  +----+-----------+---------------+
  //
  val valid_array = Reg(Vec(nSets, Bool()))
  val tag_array   = Reg(Vec(nSets, UInt(tagBits.W)))
  val pred_array  = Reg(Vec(nSets, UInt(predBits.W)))
  val plru        = RegInit(UInt((nSets-1).W), 0.U((nSets-1).W))

  //  Update
  //  Step 1: Compare
  val upd_tag = compute_tag(io.upd.bits.addr)
  val upd_vec = (valid_array zip tag_array) map { case (v, t) => v && (t === upd_tag) }

  //  Step 2: Find an invalid entry
  val has_invalids = !valid_array.reduce(_&_)
  val invalid_idx = OHToUInt(selectFirst(Reverse(Cat(valid_array))))
  val repl_idx = Mux(has_invalids, invalid_idx, get_replace_way(plru))

  //  Step 3: Update BTB
  val upd_idx = Mux(upd_vec.reduce(_|_), OHToUInt(upd_vec), repl_idx)
  val new_tag = upd_tag
  val new_pred = Wire(new FastBTBPredBundle)
  new_pred.tg_addr  := io.upd.bits.tg_addr
  new_pred.offset   := io.upd.bits.offset
  new_pred.br_type  := io.upd.bits.br_type

  for (s <- 0 until nSets) {
    when ((s.U === upd_idx) && io.upd.valid) {
      valid_array(s)  := true.B
      tag_array(s)    := new_tag
      pred_array(s)   := new_pred.asUInt
    }
  }

  //  Require
  val req_tag = compute_tag(io.req.bits.addr)
  val bypass_idx = upd_idx
  val bypass_tag = new_tag
  val bypass_pred = new_pred
  val do_bypass = RegNext(io.upd.valid && (bypass_tag === req_tag))

  val hit_vec = (valid_array zip tag_array) map { case (v, t) => v && (t === req_tag) }
  val hit_data = RegNext(Mux1H(hit_vec, pred_array)).asTypeOf(new FastBTBPredBundle)
  val resp_data = Mux(do_bypass, bypass_pred, hit_data)

  io.resp.miss    := !(do_bypass || hit_vec.reduce(_|_))
  io.resp.tg_addr := resp_data.tg_addr
  io.resp.offset  := resp_data.offset
  io.resp.br_type := resp_data.br_type

  //  Update Replacement
  when (RegNext(io.req.valid) && !io.resp.miss) {
    plru := get_next_state(plru, Mux(do_bypass, bypass_idx, OHToUInt(hit_vec)))
  }
  when (io.upd.valid) {
    plru := get_next_state(plru, upd_idx)
  }
  
  //  End
}