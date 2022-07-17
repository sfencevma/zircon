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
package zircon.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.util._

class MicroBTBReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class MicroBTBResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss      = Bool()
  val hit_way   = UInt()
  val tg_addr   = UInt(vaddrBits.W)
  val offset    = UInt(4.W)
  val br_type   = Bool()
}

class MicroBTBUpdate(idxBits: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val addr      = UInt(vaddrBits.W)
  val new_br    = Bool()
  val hit_way   = UInt(idxBits.W)
  val tg_addr   = UInt(vaddrBits.W)
  val offset    = UInt(4.W)
  val br_type   = Bool()  //  1: Branch, 2. Jump
}


class MicroBTB(nSets: Int, nWays: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new MicroBTBReq))
    val resp = Output(new MicroBTBResp)
    val upd = Flipped(Valid(new MicroBTBUpdate(log2Ceil(nWays))))
  })

  def offBits: Int = log2Ceil(icacheParams.fetchBytes)
  def tagSz = vpnBits - offBits
  def predSz = vaddrBits + offBits + 1
  def compute_idx(addr: UInt) = {
    val lgSets = log2Ceil(nSets)
    val base = offBits
    addr(base + lgSets - 1, base)
  }

  class MicroBTBPredBundle extends Bundle {
    val tg_addr = UInt(vaddrBits.W)
    val offset  = UInt(offBits.W)
    val br_type = Bool()
  }

  val valid_array = Reg(Vec(nSets, Vec(nWays, Bool())))
  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(tagSz.W)))
  val pred_array = SyncReadMem(nSets, Vec(nWays, UInt(predSz.W)))
  val plru_array = Reg(Vec(nSets, UInt((nWays-1).W)))

  //  Update
  //  PseudoLRU
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(nWays) - 1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.asUInt.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(nWays-1,1)
  }

  def get_replace_way(state: UInt) = {
    val shift_state = state << 1
    var idx = 1.U(1.W)

    for (i <- log2Up(nWays) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(nWays)-1, 0) < nWays.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(nWays)-1,0)
  }

  val upd_idx = compute_idx(io.upd.bits.addr)
  val has_invalid_way = !valid_array(upd_idx).reduce(_&_)
  val invalid_way = OHToUInt(selectFirst(Reverse(Cat(valid_array(upd_idx)))))
  val repl_way = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(upd_idx)))
  val upd_way = Mux(io.upd.bits.new_br, repl_way, io.upd.bits.hit_way)

  val new_tag = Wire(UInt(tagSz.W))
  new_tag := io.upd.bits.addr(vaddrBits - 1, offBits)

  val new_pred = Wire(new MicroBTBPredBundle)
  new_pred.tg_addr := io.upd.bits.tg_addr
  new_pred.offset := io.upd.bits.offset
  new_pred.br_type := io.upd.bits.br_type

  when (io.upd.valid) {
    for (w <- 0 until nWays) {
      when (w.U === upd_way) {
        valid_array(upd_idx)(w) := true.B
      }
    }
    tag_array.write(upd_idx, VecInit(Seq.fill(nWays){new_tag.asUInt}), UIntToOH(upd_way).asBools)
    pred_array.write(upd_idx, VecInit(Seq.fill(nWays){new_pred.asUInt}), UIntToOH(upd_way).asBools)
  }

  val s2_upd_way = RegNext(upd_way)

  //  Require
  val req_idx = compute_idx(io.req.bits.addr)
  val req_tag = io.req.bits.addr(vaddrBits-1, offBits)
  val bypass_tag = RegNext(new_tag)
  val bypass_pred = RegNext(new_pred)
  val do_bypass = RegNext(io.upd.valid && (req_idx === upd_idx))

  val read_valids = RegEnable(valid_array(req_idx), io.req.valid).zipWithIndex map { case (v, w) =>
    Mux(do_bypass && (s2_upd_way === w.U), true.B, v)
  }
  val read_tags = tag_array.read(req_idx, io.req.valid).zipWithIndex map { case (t, w) =>
    Mux(do_bypass && (s2_upd_way === w.U), new_tag, t)
  }
  val read_preds = pred_array.read(req_idx, io.req.valid).zipWithIndex map { case (d, w) =>
    Mux(do_bypass && (s2_upd_way === w.U), new_pred, d.asTypeOf(new MicroBTBPredBundle))
  }


  val hit_vec = (read_valids zip read_tags) map { case (v, t) => v && (t === RegEnable(req_tag, io.req.valid)) }
  val hit_data = Mux1H(hit_vec, read_preds)

  io.resp.miss := !(do_bypass || hit_vec.reduce(_|_))
  io.resp.hit_way := OHToUInt(hit_vec)
  io.resp.tg_addr := hit_data.tg_addr
  io.resp.offset := hit_data.offset
  io.resp.br_type := hit_data.br_type

  //  Update Replacement
  val s2_req_idx = RegNext(req_idx)
  when ((RegNext(io.req.valid) && !io.resp.miss)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), io.resp.hit_way)
  }
  when (io.upd.valid) {
    plru_array(upd_idx) := get_next_state(plru_array(upd_idx), upd_way)
  }
}
