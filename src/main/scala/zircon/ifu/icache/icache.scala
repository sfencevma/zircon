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
package zircon.ifu.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common.{BaseZirconBundle, BaseZirconModule}
import zircon.util._

case class ICacheParams (
                          nSets: Int = 64,
                          nWays: Int = 8,
                          nTLBSets: Int = 64,
                          nTLBWays: Int = 8,
                          blockBytes: Int = 64,
                          fetchBytes: Int = 16
                        ) {
  def fetchBits: Int = fetchBytes * 8
  def blockBits: Int = blockBytes * 8
}

class ICacheReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
  val ppn  = UInt(ppnBits.W)
}

class ICacheResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss = Bool()
  val data = UInt(icacheParams.fetchBits.W)
}

class ICachePTWReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(paddrBits.W)
}

class ICachePTWResp(implicit p: Parameters) extends BaseZirconBundle {
  val data  = UInt(icacheParams.blockBits.W)
  val cause = UInt(eLen.W)
}

class ICachePTW(implicit p: Parameters) extends BaseZirconBundle {
  val req   = Decoupled(new ICachePTWReq)
  val resp  = Flipped(Valid(new ICachePTWResp))
}

class ICacheSFenceReq(implicit p: Parameters) extends BaseZirconBundle {
  val invalid_all = Bool()
}

class ICache(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val sfence = Flipped(Valid(new ICacheSFenceReq))
    val req = Flipped(Valid(new ICacheReq))
    val resp = Output(new ICacheResp)
    val ptw = new ICachePTW
    val invalidate = Input(Vec(icacheParams.nTLBWays, new ITLBInvalidate))
  })


  def lgSets: Int = log2Ceil(icacheParams.nSets)
  def lgWays: Int = log2Ceil(icacheParams.nWays)
  def offBits: Int = log2Ceil(icacheParams.fetchBytes)
  def tagSz: Int = 1 + ppnBits
  def bankBits: Int = icacheParams.fetchBits
  def compute_idx(addr: UInt) = addr(offBits + lgSets - 1, offBits)
  def compute_bank(addr: UInt) = addr(offBits + lgSets + lgWays - 1, offBits + lgSets)

  class ICacheTagBundle extends Bundle {
    val valid = Bool()
    val ppn   = UInt(ppnBits.W)
  }

  val tag_array = Mem(icacheParams.nSets, Vec(icacheParams.nWays, UInt(tagSz.W)))
  val data_array = SyncReadMem(icacheParams.nSets, Vec(icacheParams.nWays, UInt(icacheParams.blockBits.W)))
  val plru_array = Mem(icacheParams.nSets, UInt((icacheParams.nWays-1).W))

  //  Require
  val req_idx = compute_idx(io.req.bits.addr)
  val req_addr = RegEnable(io.req.bits.addr, io.req.valid)
  val req_bank = compute_bank(req_addr)
  val read_tags = RegEnable(tag_array(req_idx), io.req.valid).map(_.asTypeOf(new ICacheTagBundle))
  val read_datas = data_array.read(req_idx, io.req.valid)
  val hit_vec = read_tags map { tag =>
    tag.valid && (tag.ppn === io.req.bits.ppn)
  }
  val hit_data = Mux1H(hit_vec, read_datas)
  io.resp.miss := !hit_vec.reduce(_|_)
  io.resp.data := MuxCase(0.U,
    Array(
      (req_bank === 0.U) -> hit_data(1 * bankBits - 1,            0),
      (req_bank === 1.U) -> hit_data(2 * bankBits - 1, 1 * bankBits),
      (req_bank === 2.U) -> hit_data(3 * bankBits - 1, 2 * bankBits),
      (req_bank === 3.U) -> hit_data(4 * bankBits - 1, 3 * bankBits)
    )
  )

  val s2_req_idx = compute_idx(req_addr)

  //  Refill
  io.ptw.req := DontCare

  //
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state << 1
    var idx = 1.U(1.W)
    for (i <- log2Up(icacheParams.nWays) - 1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.asUInt.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state(icacheParams.nWays-1,1)
  }

  def get_replace_way(state: UInt) = {
    val shift_state = state << 1
    var idx = 1.U(1.W)

    for (i <- log2Up(icacheParams.nWays) - 1 to 0 by -1) {
      val in_bounds = Cat(idx, 1.U << i)(log2Up(icacheParams.nWays)-1, 0) < icacheParams.nWays.U
      idx = Cat(idx, in_bounds && shift_state(idx))
    }
    idx(log2Up(icacheParams.nWays)-1,0)
  }

  val refill_idx = compute_idx(req_addr)
  val refill_tag = Wire(new ICacheTagBundle)
  val refill_data = io.ptw.resp.bits.data

  refill_tag.valid := true.B
  refill_tag.ppn := io.req.bits.ppn

  val refill_sel_tags = tag_array(refill_idx).map(_.asTypeOf(new ICacheTagBundle))
  val has_invalid_way = !refill_sel_tags.map(_.valid).reduce(_&_)
  val invalid_way = OHToUInt(selectFirst(Reverse(Cat(refill_sel_tags.map(!_.valid)))))
  val refill_way = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(refill_idx)))

  when (io.ptw.resp.valid && !io.ptw.resp.bits.cause.orR && !io.sfence.valid) {
    for (w <- 0 until icacheParams.nWays) {
      when (w.U === refill_way) {
        tag_array(refill_idx)(w) := refill_tag.asUInt
      }
    }
    data_array.write(refill_idx, VecInit(Seq.fill(icacheParams.nWays){refill_data}), UIntToOH(refill_way).asBools)
  }

  //  Invalidate
  val invalid_all = io.sfence.bits.invalid_all
  val invalid_vec = read_tags.map(tag =>
    invalid_all ||
      io.invalidate.map(w => w.valid && (w.ppn === tag.ppn)).reduce(_|_)
  )

  for (s <- 0 until icacheParams.nSets) {
    for (w <- 0 until icacheParams.nWays) {
      when (io.sfence.valid &&
        (invalid_all ||
          io.invalidate.map(i =>
            i.valid && (i.ppn === tag_array(s)(w).asTypeOf(new ICacheTagBundle).ppn)).reduce(_|_))) {
        tag_array(s)(w).asTypeOf(new ICacheTagBundle).valid := false.B
      }
    }
  }

  //  Update Replacement Valid when
  //  1.  ICache Require Valid and ICache HIT.
  //  2.  ICache Refill Valid.
  when ((RegNext(io.req.valid) && !io.resp.miss && !io.sfence.valid)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), OHToUInt(hit_vec))
  }
  when (io.ptw.resp.valid) {
    plru_array(refill_idx) := get_next_state(plru_array(refill_idx), refill_way)
  }
}