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

class LTageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class LTageResp(implicit p: Parameters) extends BaseZirconBundle {
  val taken = Bool()
}

class LTageUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr  = UInt(vaddrBits.W)
  val taken = Bool()
}

class LTagePredictor(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val req = Flipped(Valid(new LTageReq))
    val resp = Output(new LTageResp)
    val upd = Flipped(Valid(new LTageUpdate))
  })

  val loop = Module(new LoopPredictor(LoopParams()))
  val tage = Module(new TagePredictor(TageParams()))

  val do_req = io.req.valid
  val do_upd = io.upd.valid

  //  *******************************************
  //  Step 1: Read data
  loop.io.req           := io.req
  tage.io.req           := io.req
  io.resp.taken := Mux(loop.io.resp.use_loop, loop.io.resp.taken, tage.io.resp.taken)

  //  *******************************************
  //  Step 2: Update logic
  //  Loop Update
  loop.io.upd.valid           := do_upd && loop.io.resp.use_loop
  loop.io.upd.bits.addr       := io.upd.bits.addr
  loop.io.upd.bits.taken      := io.upd.bits.taken
  loop.io.upd.bits.loop_taken := loop.io.resp.taken

  //  Tage Update
  tage.io.upd.valid       := do_upd && !loop.io.resp.use_loop
  tage.io.upd.bits.addr   := io.upd.bits.addr
  tage.io.upd.bits.taken  := io.upd.bits.taken
}