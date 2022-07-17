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
import zircon.mmu._
import zircon.util._

class DTLBReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class DTLBResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss = Bool()
  val ppn  = UInt(ppnBits.W)
}

class DTLBRefill(implicit p: Parameters) extends BaseZirconBundle {
  val addr    = UInt(vaddrBits.W)
  val level   = UInt(lgPgLevels.W)
  val pte     = new PTE
}

class DTLBInvalidate(implicit p: Parameters) extends BaseZirconBundle {
  val valid = Bool()
  val ppn   = UInt(ppnBits.W)
}

class DTLBPTWReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class DTLBPTWResp(implicit p: Parameters) extends BaseZirconBundle {
  val pte   = new PTE
  val level = UInt(lgPgLevels.W)
  val cause = UInt(eLen.W)
}

class DTLBPTW(implicit p: Parameters) extends BaseZirconBundle {
  val asid = Input(UInt(asIdBits.W))
  val prv = Input(UInt(2.W))
  val req = Decoupled(new DTLBPTWReq)
  val resp = Flipped(Valid(new DTLBPTWResp))
}

class DTLB(nSets: Int, nWays: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val sfence = Flipped(Valid(new SFenceReq))
    val req = Flipped(Valid(new DTLBReq))
    val resp = Output(new DTLBResp)
    val ptw = new DTLBPTW
    val invalidate = Output(Vec(nWays, new DTLBInvalidate))
  })

  def lgSets: Int = log2Ceil(nSets)
  def lgWays: Int = log2Ceil(nWays)
  def offBits: Int = log2Ceil(icacheParams.blockBytes)
  def compute_idx(addr: UInt) = addr(offBits + lgSets - 1, offBits)
  def tagSz: Int = asIdBits + vpnBits + lgPgLevels
  def dataSz: Int = 6 + ppnBits

  class DTLBTagBundle extends Bundle {
    val asid  = UInt(asIdBits.W)
    val prv   = UInt(2.W)
    val vpn   = UInt(vpnBits.W)
    val level = UInt(lgPgLevels.W)
  }

  class DTLBDataBundle extends Bundle {
    val r   = Bool()
    val w   = Bool()
    val x   = Bool()
    val u   = Bool()
    val g   = Bool()
    val a   = Bool()
    val d   = Bool()
    val ppn = UInt(ppnBits.W)

    def leaf  = (r || (x && !w)) && a
    def sr    = leaf && r
    def sw    = leaf && w && d
    def sx    = leaf && x
    def ur    = sr && u
    def uw    = sw && u
    def ux    = sx && u
  }

  val valid_array = Mem(nSets, Vec(nWays, Bool())) // Reg(Vec(nSets, Vec(nWays, Bool())))
  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(tagSz.W)))
  val data_array = SyncReadMem(nSets, Vec(nWays, UInt(dataSz.W)))
  val plru_array = Mem(nSets, UInt((nWays-1).W)) // Reg(Vec(nSets, UInt((nWays-1).W)))

  //  Require
  def vpnHit(vpn: UInt, valid: Bool, tag: DTLBTagBundle) = {
    var tagMatch = valid
    val superpageOnly = false.B
    for (j <- 0 until pgLevels) {
      val base = vpnBits - (j + 1) * pgLevelBits
      val ignore = tag.level < j.U || superpageOnly && (j.U === (pgLevels - 1).U)
      tagMatch = tagMatch && (ignore || tag.vpn(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
    }
    tagMatch
  }

  def tagHit(asid: UInt, prv: UInt, vpn: UInt, valid: Bool, tag: DTLBTagBundle) = {
    (asid === tag.asid) && (prv === tag.prv) && vpnHit(vpn, valid, tag)
  }

  val req_idx = compute_idx(io.req.bits.addr)
  val req_addr = RegEnable(io.req.bits.addr, io.req.valid)
  val read_valids = RegEnable(valid_array(req_idx), io.req.valid)
  val read_tags = tag_array.read(req_idx).map(_.asTypeOf(new DTLBTagBundle))
  val read_datas = tag_array.read(req_idx).map(_.asTypeOf(new DTLBDataBundle))
  val hit_vec = read_valids zip read_tags map { case (v, tag) =>
    tagHit(io.ptw.asid, io.ptw.prv, req_addr(vaddrBits-1, pgIdxBits), v, tag)
  }
  val hit_data = Mux1H(hit_vec, read_datas)
  io.resp.miss := !hit_vec.reduce(_|_)
  io.resp.ppn := hit_data.ppn

  val s2_req_idx = compute_idx(req_addr)

  //  Refill
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

  //
  io.ptw.req.valid := DontCare
  io.ptw.req.bits := DontCare

  val refill_idx = compute_idx(req_addr)
  val refill_tag = Wire(new DTLBTagBundle)
  val refill_data = Wire(new DTLBDataBundle)
  val new_data = WireInit(io.ptw.resp.bits)

  refill_tag.asid   := io.ptw.asid
  refill_tag.prv    := io.ptw.prv
  refill_tag.vpn    := req_addr(vaddrBits-1, pgIdxBits)
  refill_tag.level  := new_data.level
  refill_data.r     := new_data.pte.r
  refill_data.w     := new_data.pte.w
  refill_data.x     := new_data.pte.x
  refill_data.u     := new_data.pte.u
  refill_data.g     := new_data.pte.g
  refill_data.a     := new_data.pte.a
  refill_data.d     := new_data.pte.d
  refill_data.ppn   := new_data.pte.ppn

  val refill_valids = valid_array(refill_idx)
  val has_invalid_way = !refill_valids.reduce(_&_)
  val invalid_way = OHToUInt(selectFirst(Reverse(Cat(refill_valids.map(!_)))))
  val refill_way = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(refill_idx)))

  when (io.ptw.resp.valid && !io.ptw.resp.bits.cause.orR && !io.sfence.valid) {
    valid_array(refill_idx) := VecInit(valid_array(refill_idx).zipWithIndex.map {
      case (v, i) => Mux(i.U === refill_way, true.B, v)
    })
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays) {refill_tag.asUInt}), UIntToOH(refill_way).asBools)
    data_array.write(refill_idx, VecInit(Seq.fill(nWays) {refill_data.asUInt}), UIntToOH(refill_way).asBools)
  }

  //  Invalidate
  val invalid_all = !io.sfence.bits.asid_vld && !io.sfence.bits.vpn_vld
  val invalid_with_vpn = io.sfence.bits.vpn_vld
  val invalid_with_asid = io.sfence.bits.asid_vld
  val invalid_vec = (read_valids zip read_tags) zip read_datas map { case ((v, tag), data) =>
    invalid_all ||
      (invalid_with_vpn && vpnHit(io.sfence.bits.vpn, v, tag)) ||
      (invalid_with_asid && (io.sfence.bits.asid === tag.asid) && !data.g)
  }
  when (io.sfence.valid) {
    for (w <- 0 until nWays) {
      when (invalid_vec(w)) {
        valid_array(req_idx)(w) := false.B
      }
    }
  }

  for (w <- 0 until nWays) {
    io.invalidate(w).valid := invalid_vec(w)
    io.invalidate(w).ppn := read_datas(w).ppn
  }

  //  Update Replacement Valid when
  //  1.  DTLB Require Valid and DTLB HIT
  //  2.  DTLB Refill Valid.
  when ((RegNext(io.req.valid) && !io.resp.miss && !io.sfence.valid)) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), OHToUInt(hit_vec))
  }
  when (io.ptw.resp.valid) {
    plru_array(refill_idx) := get_next_state(plru_array(refill_idx), refill_way)
  }
}