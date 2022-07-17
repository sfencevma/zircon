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
package zircon.decode.bpd

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.util._

trait HasBTBParameters {
  def nSets: Int = 64
  def nWays: Int = 4
  def lgSets: Int = log2Ceil(nSets)
  def lgWays: Int =log2Ceil(nWays)
}

class BTBBundle(implicit p: Parameters) extends BaseZirconBundle with HasBTBParameters
class BTBModule(implicit p: Parameters) extends BaseZirconModule with HasBTBParameters

class BTBReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
  val len  = Bool()
}

class BTBResp(implicit p: Parameters) extends BTBBundle {
  val miss    = Bool()
  val tg_addr = UInt(vaddrBits.W)
  val way_idx = UInt(lgWays.W)
  val is_jmp  = Bool()
  val is_br   = Bool()
  val is_call = Bool()
  val is_ret  = Bool()
}

class BTBUpdate(implicit p: Parameters) extends BTBBundle {
  val alloc   = Bool()
  val addr    = UInt(vaddrBits.W)
  val way_idx = UInt(lgWays.W)
  val tg_addr = UInt(vaddrBits.W)
  val len     = Bool()
  val is_jmp  = Bool()
  val is_br   = Bool()
  val is_call = Bool()
  val is_ret  = Bool()
}

class BTB(implicit p: Parameters) extends BTBModule {
  val io = IO(new Bundle() {
    val prv = Input(UInt(2.W))
    val req = Flipped(Valid(new BTBReq))
    val resp = Output(new BTBResp)
    val upd = Flipped(Valid(new BTBUpdate))
  })

  def SET_INDEX_LSB: Int = log2Ceil(icacheParams.fetchBytes)
  def SET_INDEX_MSB: Int = SET_INDEX_LSB + log2Ceil(nSets) - 1
  def compute_idx_and_tag(addr: UInt) = {
    (addr(SET_INDEX_MSB, SET_INDEX_LSB), addr(vaddrBits-1, SET_INDEX_MSB+1))
  }
  def makeTag(addr: UInt, prv: UInt, len: UInt) = Cat(addr, prv, len)

  def tagBits: Int = 39 + 2 + 1
  class BTBTagBundle extends Bundle {
    val addr  = UInt(39.W)
    val prv   = UInt(2.W)
    val len   = Bool()
  }

  def dataBits: Int = vaddrBits + 4
  class BTBDataBundle extends Bundle {
    val tg_addr = UInt(vaddrBits.W)
    val is_jmp  = Bool()
    val is_br   = Bool()
    val is_call = Bool()
    val is_ret  = Bool()
  }

  //
  val valid_array = Mem(nSets, Vec(nWays, Bool()))
  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(tagBits.W)))
  val data_array = SyncReadMem(nSets, Vec(nWays, UInt(dataBits.W)))
  val plru_array = Mem(nSets, UInt(log2Ceil(nWays).W))

  //  Update
  val (upd_idx, upd_tag) = compute_idx_and_tag(io.upd.bits.addr)
  val new_tag = Wire(new BTBTagBundle)
  val new_data = Wire(new BTBDataBundle)

  new_tag.addr      := upd_tag
  new_tag.prv       := io.prv
  new_tag.len       := io.upd.bits.len
  new_data.tg_addr  := io.upd.bits.tg_addr
  new_data.is_jmp   := io.upd.bits.is_jmp
  new_data.is_br    := io.upd.bits.is_br
  new_data.is_call  := io.upd.bits.is_call
  new_data.is_ret   := io.upd.bits.is_ret

  val valid_entry = Reverse(Cat(valid_array(upd_idx)))
  val has_invalid_way = !valid_entry.orR
  val invalid_way = selectFirst(valid_entry)
  val upd_way = Mux(io.upd.bits.alloc,
    Mux(has_invalid_way, OHToUInt(invalid_way), plru_array(upd_idx)), io.upd.bits.way_idx)

  for (w <- 0 until nWays) {
    when (io.upd.valid && (w.U === upd_way)) {
      valid_array(upd_idx)(w) := true.B
    }
  }

  when (io.upd.valid) {
    tag_array.write(upd_idx, VecInit(Seq.fill(nWays){new_tag.asUInt}), UIntToOH(upd_way).asBools)
    data_array.write(upd_idx, VecInit(Seq.fill(nWays){new_data.asUInt}), UIntToOH(upd_way).asBools)
  }

  //  Get Prediction
  val (req_idx, req_tag) = compute_idx_and_tag(io.req.bits.addr)
  val s2_req_tag = RegEnable(req_tag, io.req.valid)
  val s2_req_len = RegEnable(io.req.bits.len, io.req.valid)
  val s2_req_idx = RegEnable(req_idx, io.req.valid)
  val bypass_hit = Cat(req_tag, io.prv, io.req.bits.len) === Cat(upd_tag, io.prv, io.upd.bits.len)
  val do_bypass = RegNext(upd_idx === req_idx && bypass_hit && io.upd.valid)
  val bypass_data = RegNext(new_data)
  val req_valids = RegEnable(valid_array(req_idx), io.req.valid)
  val req_tags = tag_array.read(req_idx, io.req.valid).map(_.asTypeOf(new BTBTagBundle))
  val req_datas = data_array.read(req_idx, io.req.valid).map(_.asTypeOf(new BTBDataBundle))
  val req_hits = req_valids zip req_tags map { case (v, t) => makeTag(s2_req_tag, io.prv, s2_req_len) === makeTag(t.addr, t.prv, t.len) & v }
  val resp_data = Mux(do_bypass, bypass_data, Mux1H(req_hits, req_datas))

  io.resp.miss    := !(do_bypass || req_hits.reduce(_|_))
  io.resp.tg_addr := resp_data.tg_addr
  io.resp.way_idx := OHToUInt(Reverse(Cat(req_hits)))
  io.resp.is_jmp  := resp_data.is_jmp
  io.resp.is_br   := resp_data.is_br
  io.resp.is_call := resp_data.is_call
  io.resp.is_ret  := resp_data.is_ret

  //  Update PLRU
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
      val in_bounds = Cat(idx, 1.U << i)(log2Up(nWays) - 1, 0) < nWays.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(nWays) - 1, 0)
  }


  when ((RegNext(io.req.valid) & !io.resp.miss)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), io.resp.way_idx)
  }
  when (io.upd.valid) {
    plru_array(upd_idx) := get_next_state(plru_array(upd_idx), upd_way)
  }
}