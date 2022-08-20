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


/**
 * Geometric history length prediction was introduced with the O-GEHL predictor.
 * The predictor features M distinct predictor tables Ti, 0 <= i < M indexed with hash
 * funcitons of the branch and the global branch/path history.
 * Distinct history lengths are used for computing the index of the distinct tables.
 * Table T0 is indexed using the branch address. The history lengths used for computing
 * the indexing functions for tables Ti, 1 <= i < M are of the form L(i) = a^(i-1)*L(1),
 * i.e, the lengths L(i) form a geometric series.
 */
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

  //  *****************************************************************************
  //  +-----+       +------+------+-----+                      +------+------+-----+
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  | base|       | pred |  tag |  u  |   ............       | pred |  tag |  u  |
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  |     |       |      |      |     |                      |      |      |     |
  //  +-----+       +------+--+---+-----+                      +------+--+---+-----+
  //
  val meta_array = SyncReadMem(nSets, UInt(metaBits.W))

  //  Get Prediction
  val (req_tag, req_idx) = compute_tag_and_idx(io.req.bits.addr, io.hist)
  val s2_req_tag = RegEnable(req_tag, io.req.valid)

  //  Read Meta
  val read_meta = meta_array.read(req_idx, io.req.valid)
  val meta = read_meta.asTypeOf(new TageTableMeta)

  //  Response
  io.resp.cnt := meta.cnt
  io.resp.usb := meta.usb
  io.resp.hit := meta.tag === s2_req_tag

  //  Update Table
  val (upd_tag, upd_idx) = compute_tag_and_idx(io.upd.bits.addr, io.upd_hist)
  val wentry = WireInit(read_meta).asTypeOf(new TageTableMeta)
  val upd_info = io.upd.bits

  //  Update Counter.
  val wentry_cnt = Mux(upd_info.do_alloc, Mux(upd_info.taken, 4.U, 3.U),
    Mux(upd_info.do_inc_cnt || upd_info.do_dec_cnt,
      Mux(upd_info.do_inc_cnt, counterInc(upd_info.old_cnt), counterDec(upd_info.old_cnt)), upd_info.old_cnt))

  wentry.cnt := Mux(upd_info.do_flip_hi || upd_info.do_flip_lo,
    Mux(upd_info.do_flip_lo, wentry_cnt & 1.U, wentry_cnt & 2.U), wentry_cnt)

  //  Update Useful bit.
  wentry.usb := Mux(upd_info.do_alloc, 0.U,
    Mux(upd_info.do_inc_usb || upd_info.do_dec_usb,
      Mux(upd_info.do_inc_usb, counterInc(upd_info.old_usb), counterDec(upd_info.old_usb)), upd_info.old_usb))

  //  Update Tag.
  wentry.tag := upd_tag

  //  Write table.
  when (io.upd.valid) {
    meta_array(upd_idx) := wentry.asUInt
  }

  //  End
}

class TageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class TageResp(implicit p: Parameters) extends BaseZirconBundle {
  val taken = Bool()
}

class TageUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr  = UInt(vaddrBits.W)
  val taken = Bool()
}

class TagePredictor(params: TageParams)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new TageTableReq))
    val resp = Output(new TageResp)
    val upd = Flipped(Valid(new TageUpdate))
  })

  val ghist = Reg(UInt(params.ghistLength.W))
  val bim = SyncReadMem(params.bimEntries, UInt(2.W))
  val tables = params.tableInfo map {
    case (n, l, s) =>
      val t = Module(new TageTable(n, s, l))
      t.io.req := io.req
      t.io.hist := ghist
      t
  }
  val alt_better_count = Reg(UInt(4.W))
  val cnt_flip = Reg(Bool())
  val total_insts = Reg(UInt(log2Up(params.period + 1).W))

  //
  def bim_compute_idx(addr: UInt, len: Int) = {
    val nChunks = (vaddrBits + len - 1) / len
    val idx_chunks = (0 until nChunks) map {
      i => addr(min((i + 1) * len, vaddrBits) - 1, i * len)
    }
    idx_chunks.reduce(_ ^ _)
  }

  //  Update Prediction
  //  *****************************************
  //  Step 1: Read data
  val do_req = io.req.valid
  val base_req_idx = bim_compute_idx(io.req.bits.addr, log2Ceil(params.bimEntries))
  val base_cnt = bim.read(base_req_idx, do_req)
  val base_pred = base_cnt > 1.U
  val tables_resp = VecInit(tables.map(_.io.resp))
  val match_vec = tables_resp.map(_.hit)
  val select_banks_oh = selectFirst(Reverse(Cat(match_vec)), 2)
  val select_banks = select_banks_oh.map(b => Mux(b.orR, OHToUInt(b), params.tableInfo.size.U))
  val (prime_bank, alt_bank) = (select_banks(0), select_banks(1))
  val tage_pred = Wire(Bool())
  val prime_taken = Wire(Bool())
  val alt_taken = Wire(Bool())

  //  Set default
  tage_pred := base_pred
  prime_taken := false.B
  alt_taken := false.B

  //  At prediction time, the base predictor and the tagged components are
  //  accessed simultaneously. The base predictor provides a default prediction.
  //  The tagged components provided a prediction only on a tag match.
  //  The provider component as the predictor components that ultimately provides the
  //  prediction. The alternate prediction altpred as the prediction the would has occurred
  //  if there had been a miss on the provider component.
  when (prime_bank < params.tableInfo.size.U) {
    //  Hit prime bank
    when (alt_bank === params.tableInfo.size.U) {
      alt_taken := base_pred
    } .otherwise {
      alt_taken := tables_resp(alt_bank).cnt >= 3.U
    }

    when (tables_resp(prime_bank).cnt =/= 3.U ||
      tables_resp(prime_bank).cnt =/= 4.U ||
      tables_resp(prime_bank).usb =/= 0.U ||
      alt_better_count < 8.U) {
      tage_pred := tables_resp(prime_bank).cnt >= 3.U
    } .otherwise {
      tage_pred := alt_taken
    }
  }

  io.resp.taken := tage_pred

  //  *****************************************
  //  Step 2: Update logic
  val do_alloc = WireInit(false.B)
  val do_inc_cnt = WireInit(false.B)
  val do_dec_cnt = WireInit(false.B)
  val do_flip_lo = WireInit(false.B)
  val do_flip_hi = WireInit(false.B)
  val do_inc_usb = WireInit(false.B)
  val do_dec_usb = WireInit(false.B)
  val do_inc_bim = WireInit(false.B)
  val do_dec_bim = WireInit(false.B)

  //  Updating the useful counter u: The useful counter u of the provider component is
  //  updated when the alternate prediction is different from the final prediction pred.
  //  u is incremented when the actual prediction pred is correct and decremented otherwise.
  when (prime_bank < params.tableInfo.size.U) {
    when (tage_pred =/= alt_taken) {
      do_inc_usb := tage_pred === io.upd.bits.taken
      do_inc_cnt := io.upd.bits.taken
    }
  } .otherwise {
    do_inc_bim := io.upd.bits.taken
  }

  //  Updating on a correct prediction: The prediction counter of the provider component is
  //  updated.
  //  The overall prediction is incorrect: The M-i-1 uj counters are read from predictor components
  //  Tj, i < j < M. Then we apply the following rules.
  //  A) Priority for allocation
  //    1) if there exists some k, such that entry uk = 0 then a component Tk is allocated else
  //    2) the u counters from the components Tj, i < j < M are all decremented (no new entry is
  //      allocated).
  //  B) Avoiding ping-phenomenon: If two components Tj, and Tk, j < k can be allocated (i.e. uj = uk= 0)
  //    then Tj is chosen with a higher probabolity than Tk;
  //  C) Initializing the allocated entry: The allocated entry is initialized with the prediction
  //    counter set to weak correct. Counter u is initialized to 0. (i.e. strong not useful).
  //  Check whether need allocate new entry
  val need_alloc = WireInit(false.B)

  when (prime_bank < params.tableInfo.size.U) {
    when (tables_resp(prime_bank).usb === 0.U &&
      (tables_resp(prime_bank).cnt === 3.U &&
        tables_resp(prime_bank).cnt === 4.U)) {
      need_alloc := true.B
      when (tage_pred =/= alt_taken) {
        val do_inc = alt_taken === io.upd.bits.taken
        alt_better_count := Mux(do_inc, counterInc(alt_better_count), counterDec(alt_better_count))
      }
    }
  }

  //  Starting allocate a new entry
  val alt_usb_bank_oh = selectFirst(Reverse(Cat(tables_resp.map(_.usb === 0.U))))
  val alt_usb_bank = Mux(alt_usb_bank_oh(params.tableInfo.size - 1, 0).orR, OHToUInt(alt_usb_bank_oh), params.tableInfo.size.U)
  when ((!need_alloc) || (need_alloc && (tage_pred =/= io.upd.bits.taken))) {
    when (tage_pred =/= io.upd.bits.taken) {
      /** Find a entry which usb == 0, If have no entry which useful bits == 0,
       * decrease useful bits but do not allocate.
       */
      do_alloc := alt_usb_bank === params.tableInfo.size.U
    }
  }

  //  Update the rest logic
  when (io.upd.valid) {
    when (total_insts === params.period.U) {
      total_insts := 0.U
      do_flip_lo  := cnt_flip
      do_flip_hi  := ~cnt_flip
      cnt_flip    := ~cnt_flip
    } .otherwise {
      total_insts := total_insts + 1.U
    }
  }

  //  Starting update the global history
  val tage_pred_fail = Wire(Bool())
  when (io.upd.valid) {
    tage_pred_fail := io.upd.bits.taken =/= tage_pred
    ghist := Cat(ghist(params.ghistLength-2, 0), ~tage_pred_fail)
  } .otherwise {
    tage_pred_fail := false.B
  }

  //  Starting update the Bimodal table
  when (io.upd.valid && (do_dec_bim || do_inc_bim)) {
    bim.write(base_req_idx, Mux(do_dec_bim, counterInc(base_cnt), counterDec(base_cnt)))
  }

  for (w <- 0 until params.tableInfo.size) {
    val dec_usb = !do_alloc && (w.U < prime_bank)

    tables(w).io.upd.valid            := io.upd.valid
    tables(w).io.upd.bits.addr        := io.upd.bits.addr
    tables(w).io.upd.bits.taken       := io.upd.bits.taken
    tables(w).io.upd.bits.old_cnt     := tables_resp(w).cnt
    tables(w).io.upd.bits.old_usb     := tables_resp(w).usb
    tables(w).io.upd.bits.do_alloc    := do_alloc
    tables(w).io.upd.bits.do_inc_cnt  := do_inc_cnt
    tables(w).io.upd.bits.do_dec_cnt  := do_dec_cnt
    tables(w).io.upd.bits.do_flip_hi  := do_flip_hi
    tables(w).io.upd.bits.do_flip_lo  := do_flip_lo
    tables(w).io.upd.bits.do_inc_usb  := do_inc_usb
    tables(w).io.upd.bits.do_dec_usb  := do_dec_usb && dec_usb
    tables(w).io.upd_hist             := ghist
  }

  //  End
}