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
package zircon.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.ifu._
import zircon.util._

class ScanStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid = Bool()
  val inst  = UInt(instBits.W)
  val addr  = UInt(vaddrBits.W)
  val len   = Bool()
  val rl    = Bool()
  val aq    = Bool()
  val cause = UInt(eLen.W)
}

class ScanStage(plWidth: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val stall = Input(Bool())
    val req = Flipped(Valid(new IFUStageResp))
    val resp = Valid(Vec(plWidth, new ScanStageResp))
    val forward_stall = Output(Bool())
  })


  val cross_boundary = Wire(Bool())
  val left_buffer_vld = Reg(Bool())
  val left_buffer = Reg(UInt(16.W))
  val dec_done = WireInit(true.B)
  val need_2_cycles = Wire(Bool())

  when (io.flush) {
    left_buffer_vld := false.B
  } .elsewhen (io.req.valid && dec_done) {
    left_buffer_vld := cross_boundary
  }

  when (io.req.valid) {
    left_buffer := io.req.bits.data(icacheParams.fetchBits - 1, icacheParams.fetchBits - 16)
  }

  //  FSM
  val s_idle::s_dec0::s_dec1::Nil = Enum(3)
  val prev_state = RegInit(s_idle)
  val state = WireInit(prev_state)


  switch (prev_state) {
    is (s_idle) {
      when (io.req.valid) {
        state := s_dec0
        when (need_2_cycles) {
          dec_done := false.B
        } .otherwise {
          dec_done := true.B
        }
      }
    }

    is (s_dec0) {
      when (need_2_cycles) {
        state := s_dec1
        dec_done := true.B
      } .otherwise {
        when (io.req.valid) {
          state := s_dec0
        } .otherwise {
          state := s_idle
        }

        dec_done := true.B
      }
    }

    is (s_dec1) {
      state := s_idle
      dec_done := true.B
    }
  }

  when (io.flush) {
    prev_state := s_idle
  } .elsewhen (io.stall) {
    prev_state := prev_state
  } .otherwise {
    prev_state := state
  }

  def maxInsts: Int = icacheParams.fetchBytes/2
  def offBits: Int = log2Ceil(icacheParams.fetchBytes)

  //  Begin Vector
  val begin_vec = Wire(Vec(maxInsts, Bool()))
  for (i <- 0 until maxInsts) {
    if (i == 0) {
      begin_vec(i) := Mux(left_buffer_vld, false.B, io.req.bits.addr(3, 0) === 0.U)
    } else if (i == maxInsts - 1) {
      begin_vec(i) := Mux(left_buffer_vld, true.B, ((i << 1).U >= io.req.bits.addr(3, 0))) &
        Mux(begin_vec(i-1) & io.req.bits.data(16*(i-1)+1,16*(i-1)) === 3.U, false.B,
          io.req.bits.data(16*i+1,16*i) =/= 3.U)
    } else {
      begin_vec(i) := Mux(left_buffer_vld, true.B, ((i << 1).U >= io.req.bits.addr(3, 0))) &
        (!begin_vec(i-1) || (begin_vec(i-1) && io.req.bits.data(16*(i-1)+1, 16*(i-1)) =/= 3.U))
    }
  }

  val dec0_vld_vec = Fill(maxInsts, !io.flush) & Cat(Reverse(Cat(begin_vec))(maxInsts-1, 1), left_buffer_vld | begin_vec(0))

  //  Instruction Length Logic
  val insts_len = Wire(Vec(maxInsts, Bool()))
  for (i <- 0 until maxInsts) {
    insts_len(i) := io.req.bits.data(16*i+1, 16*i) === 3.U
  }

  //  Instruction Address Logic
  val base_addr = Cat(io.req.bits.addr(vaddrBits-1, offBits), 0.U(offBits.W)) - 2.U
  val insts_addr = Wire(Vec(maxInsts, UInt(vaddrBits.W)))
  for (i <- 0 until maxInsts) {
    if (i == 0) {
      insts_addr(i) := base_addr + Mux(begin_vec(i), 2.U, 0.U)
    } else {
      insts_addr(i) := base_addr + ((i + 1) * 2).U
    }
  }

  //  Instruction
  val insts = Wire(Vec(maxInsts, UInt(instBits.W)))
  for (i <- 0 until maxInsts) {
    if (i == 0) {
      insts(i) := Mux(left_buffer_vld, io.req.bits.data(16*(i+1)-1, 16*i),
        MuxCase(0.U,
          Array(
            (begin_vec(i) & !insts_len(i)) -> Cat(0.U(16.W), io.req.bits.data(16*(i+1)-1, 16*i)),
            (begin_vec(i) & insts_len(i)) -> io.req.bits.data(16*(i+2)-1,16*i)
          )
        ))
    } else if (i == maxInsts - 1) {
      insts(i) := Cat(0.U(16.W), io.req.bits.data(16*(i+1)-1,16*i))
    } else {
      insts(i) := MuxCase(0.U,
        Array(
          (begin_vec(i) & !insts_len(i)) -> Cat(0.U(16.W), io.req.bits.data(16*(i+1)-1, 16*i)),
          (begin_vec(i) & insts_len(i)) -> io.req.bits.data(16*(i+2)-1,16*i)
        )
      )
    }
  }

  //  RL Valid when
  //  1.  CSR
  //  2.  FENCE
  //  3.  Release Operation.
  def OPC_CSR = 0x73.U(7.W)
  def OPC_FENCE = 0x0f.U(7.W)
  def OPC_RL = 0x2f.U(7.W)

  val insts_rl = VecInit(insts.map(inst =>
    (inst(6, 0) === OPC_CSR) ||
      (inst(6, 0) === OPC_FENCE) ||
      (inst(6, 0) === OPC_RL && inst(25))
  ))

  //  AQ Valid when
  //  1.  Acquire Operation.
  val insts_aq = VecInit(insts.map(inst =>
    inst(6, 0) === OPC_RL && inst(26)
  ))

  //  Check
  cross_boundary := (begin_vec(maxInsts-1) && (io.req.bits.data(16*(maxInsts-1)+1, 16*(maxInsts-1)) === 3.U))
  need_2_cycles := PopCount(begin_vec) > 4.U

  //  Select logic
  val dec1_vld_vec = Reg(UInt(maxInsts.W))
  val insts_sel = selectFirst(Mux(state === s_dec0, dec0_vld_vec, dec1_vld_vec), plWidth)

  when (!io.stall) {
    dec1_vld_vec := dec0_vld_vec & ~insts_sel.reduce(_|_)
  }


  //  Response
  val resp_valid = Reg(Bool())
  val resp = Reg(Vec(plWidth, new ScanStageResp))
  val cause = Reg(UInt(eLen.W))

  for (w <- 0 until plWidth) {
    val which_inst = OHToUInt(insts_sel(w))
    when (!io.stall) {
      resp(w).valid := insts_sel(w).orR
      resp(w).inst  := insts(which_inst)
      resp(w).addr  := insts_addr(which_inst)
      resp(w).len   := insts_len(which_inst)
      resp(w).rl    := insts_rl(which_inst)
      resp(w).aq    := insts_aq(which_inst)
      resp(w).cause := io.req.bits.cause
    }
  }

  when (io.flush) {
    resp_valid := false.B
  } .elsewhen (io.stall) {
    resp_valid := resp_valid
  } .otherwise {
    resp_valid := state =/= s_idle
  }

  io.resp.valid := resp_valid
  io.resp.bits := resp
  io.forward_stall := need_2_cycles && (state === s_dec0)
}