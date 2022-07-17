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

import scala.math._

case class TageParams (
                        ghistLength: Int = 128,
                        tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128,       2,     7),
                                                                    (  128,       4,     7),
                                                                    (  256,       8,     8),
                                                                    (  256,      16,     8),
                                                                    (  128,      32,     9),
                                                                    (  128,      64,     9)),
                        period: Int = 2048,
                        bimEntries: Int = 2048
                      ) {
  def idxBits: Int = log2Up(tableInfo.size)
}

trait HasTageParameters {
  def ghistLength: Int = 128
  def tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128,       2,     7),
                                                  (  128,       4,     7),
                                                  (  256,       8,     8),
                                                  (  256,      16,     8),
                                                  (  128,      32,     9),
                                                  (  128,      64,     9))
  def period: Int = 2048
  def bimEntries: Int = 2048
  def idxBits: Int = log2Up(tableInfo.size)
}

class TageBundle(implicit p: Parameters) extends BaseZirconBundle with HasTageParameters
class TageModule(implicit p: Parameters) extends BaseZirconModule with HasTageParameters

class TageTableReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class TageTableResp(implicit p: Parameters) extends BaseZirconBundle {
  val hit = Bool()
  val cnt = UInt(3.W)
  val usb = UInt(2.W)
}

class TageTableUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr        = UInt(vaddrBits.W)
  val taken       = Bool()
  val old_cnt     = UInt(3.W)
  val old_usb     = UInt(2.W)
  val do_alloc    = Bool()
  val do_inc_cnt  = Bool()
  val do_dec_cnt  = Bool()
  val do_flip_lo  = Bool()
  val do_flip_hi  = Bool()
  val do_inc_usb  = Bool()
  val do_dec_usb  = Bool()
}


class TageTable(nSets: Int, tagBits: Int, histLength: Int)(implicit p: Parameters) extends BaseZirconModule {

  val io = IO(new Bundle() {
    val req = Flipped(Valid(new TageTableReq))
    val hist = Input(UInt(histLength.W))
    val resp = Output(new TageTableResp)
    val upd = Flipped(Valid(new TageTableUpdate))
    val upd_hist = Input(UInt(histLength.W))
  })

  def compute_folded_hist(hist: UInt, len: Int) = {
    val nChunks = (histLength + len - 1) / len
    val hist_chunks = (0 until nChunks) map {
      i => hist(min((i + 1) * len, histLength) - 1, i * len)
    }
    hist_chunks.reduce(_^_)
  }

  def compute_tag_and_idx(unhashed_idx: UInt, hist: UInt) = {
    val hashed_idx = unhashed_idx >> log2Ceil(icacheParams.fetchBytes)
    val idx_history = compute_folded_hist(hist, log2Ceil(nSets))
    val idx = (hashed_idx.asUInt ^ idx_history)(log2Ceil(nSets)-1,0)
    val tag_history = compute_folded_hist(hist, tagBits)
    val tag = ((unhashed_idx >> log2Ceil(nSets)).asUInt ^ tag_history)(tagBits-1,0)
    (tag, idx)
  }

  class TageTableMeta extends Bundle {
    val tag = UInt(tagBits.W)
    val cnt = UInt(3.W)
    val usb = UInt(2.W)
  }

  def metaBits: Int = tagBits + 5

  //
  val meta_array = SyncReadMem(nSets, UInt(metaBits.W))

  //  Update Table
  val (upd_tag, upd_idx) = compute_tag_and_idx(io.upd.bits.addr, io.upd_hist)
  val wentry = Wire(new TageTableMeta)
  val upd_info = io.upd.bits

  val wentry_cnt = Mux(upd_info.do_alloc, Mux(upd_info.taken, 4.U, 3.U),
    Mux(upd_info.do_inc_cnt || upd_info.do_dec_cnt,
      Mux(upd_info.do_inc_cnt, counterInc(upd_info.old_cnt), counterDec(upd_info.old_cnt)), upd_info.old_cnt))

  wentry.cnt := Mux(upd_info.do_flip_hi || upd_info.do_flip_lo,
    Mux(upd_info.do_flip_lo, wentry_cnt & 1.U, wentry_cnt & 2.U), wentry_cnt)

  wentry.usb := Mux(upd_info.do_alloc, 0.U,
    Mux(upd_info.do_inc_usb || upd_info.do_dec_usb, Mux(upd_info.do_inc_usb, counterInc(upd_info.old_usb), counterDec(upd_info.old_usb)),
      upd_info.old_usb))
  wentry.tag := upd_tag

  when (io.upd.valid) {
    meta_array(upd_idx) := wentry.asUInt
  }

  val s2_upd_vld = Mux(io.upd.valid, true.B, RegNext(io.upd.valid))
  val s2_upd_idx = Mux(io.upd.valid, upd_idx, RegNext(upd_idx))
  val s2_upd_entry = Mux(io.upd.valid, wentry, RegNext(wentry))

  //  Get Prediction
  val (req_tag, req_idx) = compute_tag_and_idx(io.req.bits.addr, io.hist)
  val read_meta = meta_array.read(req_idx, io.req.valid)

  val s2_req_idx = RegEnable(req_idx, io.req.valid)
  val s2_req_tag = RegEnable(req_tag, io.req.valid)

  val meta = Mux(s2_upd_vld && (s2_req_idx === s2_upd_idx),
    s2_upd_entry, read_meta.asTypeOf(new TageTableMeta))
  io.resp.cnt := meta.cnt
  io.resp.usb := meta.usb
  io.resp.hit := meta.tag === s2_req_tag
}


class TageMeta extends Bundle {
  val cnt = UInt(3.W)
  val usb = UInt(2.W)
}

class TageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class TageResp(implicit p: Parameters) extends TageBundle {
  val prime_taken = Bool()
  val alt_taken   = Bool()
  val prime_bank  = UInt(idxBits.W)
  val alt_bank    = UInt(idxBits.W)
  val bim_cnt     = UInt(2.W)
  val metas       = Vec(tableInfo.size, new TageMeta)
}

class TageUpdate(implicit p: Parameters) extends TageBundle {
  val addr        = UInt(vaddrBits.W)
  val taken       = Bool()
  val tage_taken  = Bool()
  val alt_taken   = Bool()
  val prime_bank  = UInt(idxBits.W)
  val alt_bank    = UInt(idxBits.W)
  val bim_cnt     = UInt(2.W)
  val metas       = Vec(tableInfo.size, new TageMeta)
}


class TagePredictor(implicit p: Parameters) extends TageModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new TageTableReq))
    val resp = Output(new TageResp)
    val upd = Flipped(Valid(new TageUpdate))
  })

  val ghist = Reg(UInt(ghistLength.W))
  val bim = SyncReadMem(bimEntries, UInt(2.W))
  val tables = tableInfo map {
    case (n, l, s) =>
      val t = Module(new TageTable(n, s, l))
      t.io.req := io.req
      t.io.hist := ghist
      t
  }
  val alt_better_count = Reg(UInt(4.W))
  val cnt_flip = Reg(Bool())
  val total_insts = Reg(UInt(log2Up(period + 1).W))

  //
  def bim_compute_idx(addr: UInt, len: Int) = {
    val nChunks = (vaddrBits + len - 1) / len
    val idx_chunks = (0 until nChunks) map {
      i => addr(min((i + 1) * len, vaddrBits) - 1, i * len)
    }
    idx_chunks.reduce(_ ^ _)
  }

  //  Get Prediction
  val base_req_idx = bim_compute_idx(io.req.bits.addr, log2Ceil(bimEntries))
  val base_cnt = bim.read(base_req_idx, io.req.valid)
  val base_pred = base_cnt > 1.U
  val tables_resp = VecInit(tables.map(_.io.resp))
  val match_vec = tables_resp.map(_.hit)
  val select_banks_oh = selectFirst(Reverse(Cat(match_vec)), 2)
  val select_banks = select_banks_oh.map(b => Mux(!b.orR, tableInfo.size.U, OHToUInt(b)))
  val (prime_bank, alt_bank) = (select_banks(0), select_banks(1))
  val tage_pred = WireInit(base_pred)
  val prime_taken = WireInit(false.B)
  val alt_taken = WireInit(false.B)
  val prime_which_bank = hashIdx(prime_bank)
  val alt_which_bank = hashIdx(alt_bank)

  when(prime_bank < tableInfo.size.U) {
    //  Hit prime bank
    when(alt_bank === tableInfo.size.U) {
      alt_taken := base_pred
    }.otherwise {
      //  Tage Taken
      when(tables_resp(alt_which_bank).cnt >= 3.U) {
        alt_taken := true.B
      }.otherwise {
        alt_taken := false.B
      }
    }

    when(tables_resp(prime_which_bank).cnt =/= 3.U ||
      tables_resp(prime_which_bank).cnt =/= 4.U ||
      tables_resp(prime_which_bank).usb =/= 0.U ||
      alt_better_count < 8.U) {
      when(tables_resp(prime_which_bank).cnt >= 3.U) {
        tage_pred := true.B
      }.otherwise {
        tage_pred := false.B
      }
    }.otherwise {
      tage_pred := alt_taken
    }
  }.otherwise {
    tage_pred := base_pred
  }

  //  Response
  val resp = Wire(new TageResp)
  resp.prime_taken := prime_taken
  resp.alt_taken := alt_taken
  resp.prime_bank := prime_which_bank
  resp.alt_bank := alt_which_bank
  resp.bim_cnt := base_cnt

  for (t <- 0 until tableInfo.size) {
    resp.metas(t).cnt := tables_resp(t).cnt
    resp.metas(t).usb := tables_resp(t).usb
  }
  io.resp := resp

  //  Update
  val upd_info = WireInit(io.upd.bits)
  val do_alloc = WireInit(false.B)
  val do_inc_cnt = WireInit(false.B)
  val do_dec_cnt = WireInit(false.B)
  val do_flip_lo = WireInit(false.B)
  val do_flip_hi = WireInit(false.B)
  val do_inc_usb = WireInit(false.B)
  val do_dec_usb = WireInit(false.B)
  val do_inc_bim = WireInit(false.B)
  val do_dec_bim = WireInit(false.B)

  when(upd_info.prime_bank < tableInfo.size.U) {
    when(upd_info.tage_taken =/= upd_info.alt_taken) {
      //  Update useful bit
      when(upd_info.tage_taken === upd_info.taken) {
        do_inc_usb := true.B
      }.otherwise {
        do_dec_usb := true.B
      }

      //  Update counter
      when(upd_info.taken) {
        do_inc_cnt := true.B
      }.otherwise {
        do_dec_cnt := false.B
      }
    }
  }.otherwise {
    when(upd_info.taken) {
      do_inc_bim := true.B
    }.otherwise {
      do_dec_bim := true.B
    }
  }

  //  Check if need allocate new entry.
  val need_alloc = WireInit(false.B)
  val upd_prime_bank = hashIdx(upd_info.prime_bank)
  val upd_alt_bank = hashIdx(upd_info.alt_bank)

  when(upd_info.prime_bank < tableInfo.size.U) {
    //  Update alt_better_counter
    when(upd_info.metas(upd_prime_bank).usb === 0.U &&
      (upd_info.metas(upd_prime_bank).cnt === 3.U ||
        upd_info.metas(upd_prime_bank).cnt === 4.U)) {
      need_alloc := true.B
      when(upd_info.tage_taken =/= upd_info.alt_taken) {
        val do_inc = upd_info.alt_taken === upd_info.taken
        alt_better_count := Mux(do_inc, counterInc(alt_better_count), counterDec(alt_better_count))
      }
    }
  }

  //  Starting allocate a new entry.
  //  Find useful bit == 0
  val alt_usb_bank_oh = selectFirst(Reverse(Cat(upd_info.metas.map(_.usb === 0.U))))
  val alt_usb_bank = Mux(!alt_usb_bank_oh.orR, tableInfo.size.U, OHToUInt(alt_usb_bank_oh))
  when((!need_alloc) || (need_alloc && (upd_info.tage_taken =/= upd_info.taken))) {
    when(upd_info.tage_taken =/= upd_info.taken) {
      when(alt_usb_bank < tableInfo.size.U) {
        /** Find a entry which usb == 0, If have no entry which useful bits == 0,
         * decrease useful bits but do not allocate.
         */
        do_alloc := false.B
      }.otherwise {
        do_alloc := true.B
      }
    }
  }

  //  Updating the reset logic
  when(io.upd.valid) {
    when(total_insts === period.U) {
      total_insts := 0.U
      do_flip_lo := cnt_flip
      do_flip_hi := ~cnt_flip
      cnt_flip := ~cnt_flip
    }.otherwise {
      total_insts := total_insts + 1.U
    }
  }

  //  Starting update the global history
  val tage_pred_fail = WireInit(false.B)
  when(io.upd.valid) {
    when(io.upd.bits.taken =/= io.upd.bits.tage_taken) {
      tage_pred_fail := true.B
      ghist := Cat(ghist(ghistLength - 2, 0), false.B)
    }.otherwise {
      tage_pred_fail := false.B
      ghist := Cat(ghist(ghistLength - 2, 0), true.B)
    }
  }

  //  Starting update the BIM table
  val upd_bim_idx = bim_compute_idx(io.upd.bits.addr, log2Ceil(bimEntries))
  when(io.upd.valid && (do_dec_bim || do_inc_bim)) {
    bim(upd_bim_idx) := Mux(do_inc_bim, counterInc(upd_info.bim_cnt), counterDec(upd_info.bim_cnt))
  }

  for (w <- 0 until tableInfo.size) {
    val dec_usb = !do_alloc && (w.U < hashIdx(upd_info.prime_bank))

    tables(w).io.upd.valid := io.upd.valid
    tables(w).io.upd.bits.addr := upd_info.addr
    tables(w).io.upd.bits.taken := upd_info.taken
    tables(w).io.upd.bits.old_cnt := upd_info.metas(w).cnt
    tables(w).io.upd.bits.old_usb := upd_info.metas(w).usb
    tables(w).io.upd.bits.do_alloc := do_alloc
    tables(w).io.upd.bits.do_inc_cnt := do_inc_cnt
    tables(w).io.upd.bits.do_dec_cnt := do_dec_cnt
    tables(w).io.upd.bits.do_flip_hi := do_flip_hi
    tables(w).io.upd.bits.do_flip_lo := do_flip_lo
    tables(w).io.upd.bits.do_inc_usb := do_inc_usb
    tables(w).io.upd.bits.do_dec_usb := do_dec_usb && dec_usb
    tables(w).io.upd_hist := ghist
  }

}