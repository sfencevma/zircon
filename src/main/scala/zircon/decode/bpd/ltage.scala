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

class LTageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class LTageResp(implicit p: Parameters) extends BaseZirconBundle {
  val loop = new LoopResp
  val tage = new TageResp
}

class LTageUpdate(implicit p: Parameters) extends TageBundle {
  val taken       = Bool()
  val addr        = UInt(vaddrBits.W)
  val loop_taken  = Bool()
  val tage_taken  = Bool()
  val use_loop    = Bool()
  val alt_taken   = Bool()
  val prime_bank  = UInt(idxBits.W)
  val alt_bank    = UInt(idxBits.W)
  val bim_cnt     = UInt(2.W)
  val metas       = Vec(tableInfo.size, new TageMeta)
}

class LTagePredictor(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new LTageReq))
    val resp = Output(new LTageResp)
    val upd = Flipped(Valid(new LTageUpdate))
  })

  val loop = Module(new LoopPredictor(LoopParams()))
  val tage = Module(new TagePredictor())

  //
  loop.io.req := io.req
  tage.io.req := io.req
  io.resp.loop := loop.io.resp
  io.resp.tage := tage.io.resp

  //  Update
  val upd = WireInit(io.upd.bits)
  val loop_upd = Wire(new LoopUpdate)

  loop_upd.taken := io.upd.bits.taken
  loop_upd.loop_taken := io.upd.bits.loop_taken
  loop_upd.addr := io.upd.bits.addr
  loop.io.upd.valid := io.upd.valid && io.upd.bits.use_loop
  loop.io.upd.bits := loop_upd

  val tage_upd = Wire(new TageUpdate)
  tage_upd.addr       := io.upd.bits.addr
  tage_upd.taken      := io.upd.bits.taken
  tage_upd.tage_taken := io.upd.bits.tage_taken
  tage_upd.alt_taken  := io.upd.bits.alt_taken
  tage_upd.prime_bank := io.upd.bits.prime_bank
  tage_upd.alt_bank   := io.upd.bits.alt_bank
  tage_upd.bim_cnt    := io.upd.bits.bim_cnt
  tage_upd.metas      := io.upd.bits.metas
  tage.io.upd.valid   := io.upd.valid && !io.upd.bits.use_loop
  tage.io.upd.bits    := tage_upd
}