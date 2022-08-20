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

class PredictorReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
  val len  = Bool()
}

class PredictorResp(implicit p: Parameters) extends BaseZirconBundle {
  val is_jmp  = Bool()
  val is_br   = Bool()
  val is_call = Bool()
  val is_ret  = Bool()
  val taken   = Bool()
  val tg_addr = UInt(vaddrBits.W)
}

class PredictorInfo(implicit p: Parameters) extends PredictorResp

class PredictorUpdate(implicit p: Parameters) extends BaseZirconBundle {
  val addr    = UInt(vaddrBits.W)
  val tg_addr = UInt(vaddrBits.W)
  val len     = Bool()
  val is_jmp  = Bool()
  val is_br   = Bool()
  val is_call = Bool()
  val is_ret  = Bool()
  val taken   = Bool()
}

class Predictor(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val stall = Input(Bool())
    val prv = Input(UInt(2.W))
    val req = Flipped(Valid(new PredictorReq))
    val resp = Output(new PredictorInfo)
    val upd = Flipped(Decoupled(new PredictorUpdate))
  })


  val btb = Module(new BTB(128, 2))
  val ras = Module(new RAS(32))
  val ltage = Module(new LTagePredictor())

  val s2_vld = Reg(Bool())
  val s2_addr = Reg(UInt(vaddrBits.W))
  val s2_next_addr = s2_addr + Mux(io.req.bits.len, 2.U, 4.U)
  val s2_len = Reg(Bool())

  //  FSM description:
  //  s_idle    : IDLE state.
  //  s_read    : Read data.
  //  s_upd     : Updating
  //  s_done    : Update done.
  val s_idle::s_read::s_upd::s_done::Nil = Enum(4)
  val state = RegInit(s_idle)

  //
  val replay_vld = Reg(Bool())
  val replay_addr = Reg(UInt(vaddrBits.W))
  val replay_len = Reg(Bool())


  switch (state) {
    is (s_idle) {
      when (io.upd.valid) {
        replay_vld := s2_vld
        replay_addr := s2_addr
        replay_len := s2_len
        state := s_read
      }
    }
    is (s_read) {
      state := s_upd
    }
    is (s_upd) {
      state := s_done
    }
    is (s_done) {
      state := s_idle
    }
  }

  val state_is_read = state === s_read
  val state_is_upd = state === s_upd
  val state_is_done = state === s_done


  //  ************************************************
  //  Step 1: Read data
  btb.io.prv            := io.prv
  btb.io.req.valid      := state_is_read || io.req.valid || (state_is_done && replay_vld)
  btb.io.req.bits.addr  := Mux(io.upd.valid, io.upd.bits.addr,
                            Mux(state_is_done && replay_vld, replay_addr, io.req.bits.addr))
  btb.io.req.bits.len   := Mux(io.upd.valid, io.upd.bits.len,
                            Mux(state_is_done && replay_vld, replay_len, io.req.bits.len))
  ltage.io.req.valid    := btb.io.req.valid
  ltage.io.req.bits.addr:= btb.io.req.bits.addr

  //  Predictor Response Taken Valid when
  //  1.  Stage 2 Valid.
  //  2.  BTB is Hit.
  //  3.  If it's non-PC relative instruction (e.g. beq) and ltage predict taken; else
  //      it's PC relative instruction (e.g. jal).
  io.resp.taken := s2_vld &&
                    !btb.io.resp.miss &&
                    (btb.io.resp.is_jmp || (btb.io.resp.is_br && ltage.io.resp.taken))

  //  Predictor Predict Target Address
  //  1.  If it's CALL instruction, if btb is hit, choose it.
  //  2.  If it's RET instruction, choose RAS target address.
  //  3.  Otherwise, choose BTB target address.
  val is_call = btb.io.resp.is_call
  val is_ret  = btb.io.resp.is_ret
  io.resp.tg_addr := Mux(is_call && !btb.io.resp.miss, btb.io.resp.tg_addr,
    Mux(is_ret, ras.io.read_addr, btb.io.resp.tg_addr))

  io.resp.is_br   := btb.io.resp.is_br
  io.resp.is_jmp  := btb.io.resp.is_jmp
  io.resp.is_call := btb.io.resp.is_call
  io.resp.is_ret  := btb.io.resp.is_ret

  //  RAS
  ras.io.read_vld   := s2_vld && is_ret
  ras.io.write_vld  := s2_vld && is_call
  ras.io.write_addr := s2_next_addr

  //  ************************************************
  //  Step 2: Update logic
  btb.io.upd.valid        := state_is_upd
  btb.io.upd.bits.alloc   := !btb.io.resp.miss
  btb.io.upd.bits.addr    := io.upd.bits.addr
  btb.io.upd.bits.tg_addr := io.upd.bits.tg_addr
  btb.io.upd.bits.len     := io.upd.bits.len
  btb.io.upd.bits.is_jmp  := io.upd.bits.is_jmp
  btb.io.upd.bits.is_br   := io.upd.bits.is_br
  btb.io.upd.bits.is_call := io.upd.bits.is_call
  btb.io.upd.bits.is_ret  := io.upd.bits.is_ret

  ltage.io.upd.valid      := state_is_upd
  ltage.io.upd.bits.taken := io.upd.bits.taken
  ltage.io.upd.bits.addr  := io.upd.bits.addr

  //  ************************************************
  //  Step 3: Response
  io.upd.ready := state_is_done

  //  ************************************************
  //  Replay
  when (state_is_done) {
    s2_vld := replay_vld
  } .elsewhen (!io.stall) {
    s2_vld := io.req.valid
  }

  when (state_is_done) {
    s2_addr := replay_addr
    s2_len  := replay_len
  } .elsewhen (!io.stall) {
    s2_addr := io.req.bits.addr
    s2_len  := io.req.bits.len
  }
}