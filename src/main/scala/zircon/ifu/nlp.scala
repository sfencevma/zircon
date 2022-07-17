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

case class NextLinePredictorParams (
                                     nBims: Int = 2048,
                                     nUBTBSets: Int = 128,
                                     nUBTBWays: Int = 2
                                   ) {
  def lgUBTBSets: Int = log2Ceil(nUBTBSets)
  def lgUBTBWays: Int = log2Ceil(nUBTBWays)
  def lgBims: Int = log2Ceil(nBims)
}

trait HasNextLinePredictorParameters {
  def nBims: Int = 2048
  def nUBTBSets: Int = 128
  def nUBTBWays: Int = 2
  def lgUBTBSets: Int = log2Ceil(nUBTBSets)
  def lgUBTBWays: Int = log2Ceil(nUBTBWays)
  def lgBims: Int = log2Ceil(nBims)
}

class NextLinePredictorBundle(implicit p: Parameters) extends BaseZirconBundle with HasNextLinePredictorParameters
class NextLinePredictorModule(implicit p: Parameters) extends BaseZirconModule with HasNextLinePredictorParameters

class NextLinePredInfo(implicit p: Parameters) extends NextLinePredictorBundle {
  val miss          = Bool()
  val ubtb_hit_way  = UInt(lgUBTBWays.W)
  val ubtb_tg_addr  = UInt(vaddrBits.W)
  val ubtb_offset   = UInt(4.W)
  val ubtb_br_type  = Bool()
  val bim_bim       = UInt(2.W)
}

class NextLinePredictorReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class NextLinePredictorResp(implicit p: Parameters) extends BaseZirconBundle {
  val predinfo  = new NextLinePredInfo
}

class NextLinePredictorUpdate(implicit p: Parameters) extends NextLinePredictorBundle {
  val addr          = UInt(vaddrBits.W)
  val ubtb_new_br   = Bool()
  val ubtb_hit_way  = UInt(lgUBTBWays.W)
  val ubtb_tg_addr  = UInt(vaddrBits.W)
  val ubtb_offset   = UInt(4.W)
  val ubtb_br_type  = Bool()
  val bim_old_bim   = UInt(2.W)
  val bim_taken     = Bool()
}

class NextLinePredictor(implicit p: Parameters) extends NextLinePredictorModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val stall = Input(Bool())
    val req = Flipped(Valid(new NextLinePredictorReq))
    val upd = Flipped(Valid(new NextLinePredictorUpdate))
    val rdt = Valid(new PCGenRediret)
    val resp = Output(new NextLinePredictorResp)
  })

  val bim = Module(new BIM(nBims))
  val ubtb = Module(new MicroBTB(nUBTBSets, nUBTBWays))

  val nlp_vld = Reg(Bool())
  when (io.flush) {
    nlp_vld := false.B
  } .elsewhen (io.stall) {
    nlp_vld := nlp_vld
  } .otherwise {
    nlp_vld := io.req.valid
  }

  //  Next-Line Prediction Response
  //  Next-Line Prediction Valid when
  //  1. NLP Valid
  //  2. NLP Hit an Array.
  //  3. Block Has an UnCondition Instruction.
  //  4. Block Has an Condition Instruction and Bimodal Predict TAKEN.
  io.rdt.valid := (nlp_vld &&
    !ubtb.io.resp.miss &&
    ((ubtb.io.resp.br_type && bim.io.resp.bim(1)) || !ubtb.io.resp.br_type))
  io.rdt.bits.addr := ubtb.io.resp.tg_addr

  //
  val resp = Reg(new NextLinePredictorResp)
  when (nlp_vld && !io.stall) {
    resp.predinfo.miss          := ubtb.io.resp.miss
    resp.predinfo.ubtb_hit_way  := ubtb.io.resp.hit_way
    resp.predinfo.ubtb_tg_addr  := ubtb.io.resp.tg_addr
    resp.predinfo.ubtb_offset   := ubtb.io.resp.offset
    resp.predinfo.ubtb_br_type  := ubtb.io.resp.br_type
    resp.predinfo.bim_bim       := bim.io.resp.bim
  }
  io.resp := resp

  //  Bimodal Update Valid when
  //  1. Decode Stage Update Valid.
  bim.io.upd.valid        := io.upd.valid
  bim.io.upd.bits.addr    := io.upd.bits.addr
  bim.io.upd.bits.old_bim := io.upd.bits.bim_old_bim
  bim.io.upd.bits.taken   := io.upd.bits.bim_taken

  //  Bimodal Require Valid when
  //  1. PCGen Valid
  bim.io.req := io.req

  //  MicroBTB Update Valid when
  //  1.  Decode Stage Update Valid.
  ubtb.io.upd.valid         := io.upd.valid
  ubtb.io.upd.bits.new_br   := io.upd.bits.ubtb_new_br
  ubtb.io.upd.bits.hit_way  := io.upd.bits.ubtb_hit_way
  ubtb.io.upd.bits.addr     := io.upd.bits.addr
  ubtb.io.upd.bits.tg_addr  := io.upd.bits.ubtb_tg_addr
  ubtb.io.upd.bits.offset   := io.upd.bits.ubtb_offset
  ubtb.io.upd.bits.br_type  := io.upd.bits.ubtb_br_type

  //  MicroBTB Require Valid when
  //  1.  PCGen Valid
  ubtb.io.req := io.req
}
