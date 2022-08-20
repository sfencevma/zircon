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
import zircon.mmu._


class DTLBReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class DTLBResp(implicit p: Parameters) extends BaseZirconBundle {
  val miss = Bool()
  val ppn  = UInt(ppnBits.W)
}

class DTLBRefill(implicit p: Parameters) extends BaseZirconBundle {
  val addr  = UInt(vaddrBits.W)
  val level = UInt(lgPgLevels.W)
  val pte   = new PTE
}

class DTLBInvalidate(implicit p: Parameters) extends BaseZirconBundle {
  val valid = Bool()
  val ppn   = UInt(ppnBits.W)
}

class DTLBPTWReq(implicit p: Parameters) extends PTWReq
class DTLBPTWResp(implicit p: Parameters) extends PTWResp

class DTLBPTW(implicit p: Parameters) extends BaseZirconBundle {
  val req  = Decoupled(new DTLBPTWReq)
  val resp = Flipped(Decoupled(new DTLBPTWResp))
}

class DTLB(nSets: Int, nWays: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val stall = Input(Bool())
    val sfence = Flipped(Decoupled(new SFenceReq))
    val ptbr = Input(new PTBR)
    val prv  = Input(UInt(2.W))

    val req = Flipped(Valid(new DTLBReq))
    val resp = Output(new DTLBResp)
    val ptw = new DTLBPTW
    val invalidate = Output(Vec(nWays, new DTLBInvalidate))
  })

  def lgSets: Int = log2Ceil(nSets)
  def lgWays: Int = log2Ceil(nWays)
  def offBits: Int = log2Ceil(icacheParams.blockBytes)
  def compute_idx(addr: UInt) = addr(offBits + lgSets - 1, offBits)
  def tagBits: Int = asIdBits + vpnBits + lgPgLevels
  def dataBits: Int = 6 + ppnBits

  /*
 * The translation is successful. The translated physical address is given as follows:
 * 1. pa.pgoff = va.pgoff.
 * 2. If i > 0, then this is a superpage translation and pa.ppn[i − 1 : 0] = va.vpn[i − 1 : 0].
 * 3. pa.ppn[LEVELS − 1 : i] = pte.ppn[LEVELS − 1 : i].
 */
  def vpnHit(vpn: UInt, valid: Bool, tag: DTLBTagBundle) = {
    var tagMatch = valid
    val superpageOnly = false.B
    for (j <- 0 until pgLevels) {
      val base = vpnBits - (j + 1) * pgLevelBits
      val ignore = tag.level < j.U || superpageOnly && (j.U === (pgLevels-1).U)
      tagMatch = tagMatch && (ignore || tag.vpn(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
    }
    tagMatch
  }

  def tagHit(asid: UInt, prv: UInt, vpn: UInt, valid: Bool, tag: DTLBTagBundle) = {
    (asid === tag.asid) && (prv === tag.prv) && vpnHit(vpn, valid, tag)
  }

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


  //  ****************************************************
  //  DTLB Field
  //  +----+--------------+--------------------+-------+ +--------+
  //  | V  |     TAG      |        DATA        |  level| |  PLRU  |
  //  +----+--------------+--------------------+-------+ +--------+
  //  | 1  |    tagBits   |       dataBits     | lgPg  | |nWays-1 |
  //  +----+--------------+--------------------+-------+ +--------+
  //

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

  val valid_array = Mem(nSets, Vec(nWays, Bool()))
  val tag_array   = SyncReadMem(nSets, Vec(nWays, UInt(tagBits.W)))
  val data_array  = SyncReadMem(nSets, Vec(nWays, UInt(dataBits.W)))
  val plru_array  = Mem(nSets, UInt((nWays-1).W))


  //  DTLB FSM
  val s_ready::s_request::s_wait::s_tag_req::s_invalidate::Nil = Enum(5)
  val state = RegInit(s_ready)

  //  DTLB Require
  val do_req = io.req.valid || state === s_tag_req
  val req_idx = compute_idx(io.req.bits.addr)
  val req_addr = RegEnable(io.req.bits.addr, do_req)
  val read_valids = RegEnable(valid_array(req_idx), do_req)
  val read_tags = tag_array.read(req_idx, do_req).map(_.asTypeOf(new DTLBTagBundle))
  val read_datas = data_array.read(req_idx, do_req).map(_.asTypeOf(new DTLBDataBundle))
  val hit_vec = read_valids zip read_tags map { case (v, tag) =>
    tagHit(io.ptbr.asid, io.prv, req_addr(vaddrBits-1, pgIdxBits), v, tag)
  }
  val hit_data = Mux(io.ptw.resp.fire, io.ptw.resp.bits.pte.ppn, Mux1H(hit_vec, read_datas).ppn)
  val hold_ena = io.ptw.resp.fire || RegNext(do_req)
  val hold_data = HoldUnless(hit_data, hold_ena)
  val hold_miss = HoldUnless(!hit_vec.reduce(_|_) & !io.ptw.resp.fire, hold_ena)
  io.resp.miss := hold_miss
  io.resp.ppn  := hold_data


  //  DTLB FSM
  val last_inv = Wire(Bool())
  val invalid_index = RegInit(UInt(log2Ceil(dcacheParams.nTLBSets).W), 0.U)
  val invalid_done = RegInit(Bool(), false.B)

  invalid_done := false.B
  when (RegNext(io.req.valid) && io.resp.miss) {
    state := s_request
  }
  when (state === s_request) {
    when (io.sfence.valid) { state := s_tag_req }
    when (io.ptw.req.ready) { state := Mux(io.sfence.valid, s_tag_req, s_wait) }
    when (io.kill) { state := s_ready }
  }
  when (state === s_wait) {
    state := Mux(io.sfence.valid, s_tag_req, Mux(io.ptw.resp.fire, s_ready, state))
  }
  when (state === s_tag_req) {
    state := s_invalidate
  }
  when (state === s_invalidate) {
    state := Mux(last_inv, s_ready, s_tag_req)
    invalid_done := last_inv
  }

  //  PTW Request
  io.ptw.req.valid      := state === s_request
  io.ptw.req.bits.addr  := req_addr
  io.ptw.resp.ready     := !io.stall

  //  Invalidation
  when (state === s_invalidate) {
    invalid_index := invalid_index + 1.U
  }
  last_inv := invalid_index === ((1 << log2Ceil(icacheParams.nTLBSets))-1).U

  //  DTLB Refill
  val s2_req_idx = compute_idx(req_addr)
  val refill_idx  = s2_req_idx
  val refill_tag  = Wire(new DTLBTagBundle)
  val refill_data = Wire(new DTLBDataBundle)
  val new_data    = WireInit(io.ptw.resp.bits)

  refill_tag.asid   := io.ptbr.asid
  refill_tag.prv    := io.prv
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

  val refill_valids   = valid_array(refill_idx)
  val has_invalid_way = !refill_valids.reduce (_&_)
  val invalid_way     = OHToUInt(selectFirst(Reverse(Cat(refill_valids.map(!_)))))
  val refill_way      = Mux(has_invalid_way, invalid_way, get_replace_way(plru_array(refill_idx)))

  when (io.ptw.resp.fire && !io.ptw.resp.bits.cause.orR && !io.sfence.valid) {
    valid_array(refill_idx) := VecInit(valid_array(refill_idx).zipWithIndex map {
      case (v, i) => (i.U === refill_way) || v
    })
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays) {refill_tag.asUInt}), UIntToOH(refill_way).asBools)
    data_array.write(refill_idx, VecInit(Seq.fill(nWays) {refill_data.asUInt}), UIntToOH(refill_way).asBools)
  }

  //  Invalidate
  /*  1. If rs1=x0 and rs2=x0, the fence orders all reads and writes made to any
   *     level of the page tables, for all address spaces. The fence also invalidates
   *     all address-translation cache entries, for all address spaces.
   *
   *  2. If rs1=x0 and rs2=x0, the fence orders all reads and writes made to any
   *     level of the page tables, but only for the address space identified by
   *     integer register rs2. Accesses to global mappings are not ordered.
   *     The fence also invalidates all address-translation cache entries matching
   *     the address space identified by integer register rs2, except for entries
   *     containing global mappings.
   *
   *  3. If rs1=x0 and rs2=x0, the fence orders only reads and writes made to
   *     leaf page table entries corresponding to the virtual address in rs1, for
   *     all address spaces. The fence also invalidates all address-translation
   *     cache entries that contain leaf page table entries corresponding to
   *     the virtual address in rs1, for all address spaces.
   *
   *  4. If rs1=x0 and rs2=x0, the fence orders only reads and writes made to
   *     leaf page table entries corresponding to the virtual address in rs1,
   *     for the address space identified by integer register rs2. Accesses to
   *     global mappings are not ordered. The fence also invalidates all address
   *     translation cache entries that contain leaf page table entries corresponding
   *     to the virtual address in rs1 and that match the address space identified by
   *     integer register rs2, except for entries containing global mappings.
   */
  val invalid_all = !io.sfence.bits.asid_vld && !io.sfence.bits.vpn_vld
  val invalid_with_vpn = io.sfence.bits.vpn_vld
  val invalid_with_asid = io.sfence.bits.asid_vld
  val invalid_vec = (read_valids zip read_tags) zip read_datas map { case ((v, tag), data) =>
    invalid_all ||
      (invalid_with_vpn && vpnHit(io.sfence.bits.vpn, v, tag)) ||
      (invalid_with_asid && (io.sfence.bits.asid === tag.asid) && !data.g)
  }

  for (w <- 0 until nWays) {
    val invalid_vld = invalid_vec(w) && (state === s_invalidate)
    when (invalid_vld) {
      valid_array(req_idx)(w) := false.B
    }
    io.invalidate(w).valid  := invalid_vld
    io.invalidate(w).ppn    := read_datas(w).ppn
  }
  io.sfence.ready := invalid_done

  //  Update Replacement Valid when
  //  1.  DTLB Require Valid and DTLB HIT.
  //  2.  DTLB Refill Valid.
  when (RegNext(io.req.valid) && !io.resp.miss && !io.sfence.valid) {
    plru_array(s2_req_idx) := get_next_state(plru_array(s2_req_idx), OHToUInt(hit_vec))
  }
  when (io.ptw.resp.fire) {
    plru_array(refill_idx) := get_next_state(plru_array(refill_idx), refill_way)
  }

  //  End
}