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
package zircon.exu.iu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.issue._


class DIV(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val req = Input(new IssueResp)
    val resp = Output(new RobExecReq)
    val writeback_req = Output(new IssueWriteBack)
    val stall = Output(Bool())
  })

  val is_div = io.req.uop.uopc === UOP_DIV
  val lrs1_sign = !io.req.uop.usign
  val lrs1 = MuxCase(io.req.uop.lrs1,
    Array(
      (io.req.uop.dw === DW_8) -> Cat(Fill(xLen-8, lrs1_sign & io.req.uop.lrs1(7, 0)), io.req.uop.lrs1(7, 0)),
      (io.req.uop.dw === DW_16) -> Cat(Fill(xLen-16, lrs1_sign & io.req.uop.lrs1(15,0)), io.req.uop.lrs1(15,0)),
      (io.req.uop.dw === DW_32) -> Cat(Fill(xLen-32, lrs1_sign & io.req.uop.lrs1(31,0)), io.req.uop.lrs1(31,0))
    )
  )
  val lrs1_sign_bit = Mux(io.req.uop.usign, 0.U, lrs1(xLen-1))
  val lrs2_sign = !io.req.uop.usign
  val lrs2 = MuxCase(io.req.uop.lrs2,
    Array(
      (io.req.uop.dw === DW_8) -> Cat(Fill(xLen-8, lrs2_sign & io.req.uop.lrs2(7, 0)), io.req.uop.lrs2(7, 0)),
      (io.req.uop.dw === DW_16) -> Cat(Fill(xLen-16, lrs2_sign & io.req.uop.lrs2(15,0)), io.req.uop.lrs2(15,0)),
      (io.req.uop.dw === DW_32) -> Cat(Fill(xLen-32, lrs2_sign & io.req.uop.lrs2(31,0)), io.req.uop.lrs2(31,0))
    )
  )
  val lrs2_sign_bit = Mux(io.req.uop.usign, 0.U, lrs2(xLen-1))

  //  FSM
  val s_idle::s_div::s_remd_chck::s_quot_corr::s_remd_corr::Nil = Enum(5)
  val state = RegInit(s_idle)
  val next_state = WireInit(state)

  val state_is_0th = state === s_idle
  val state_is_div = state === s_div
  val state_is_remd_chck = state === s_remd_chck
  val state_is_quot_corr = state === s_quot_corr
  val state_is_remd_corr = state === s_remd_corr

  val cycle = RegInit(UInt(7.W), 0.U(7.W))
  val cycle_0th = state_is_0th
  val last_cycle = cycle === 64.U
  when (state_is_0th && (next_state === s_div)) {
    cycle := 1.U
  } .elsewhen (state_is_div & !last_cycle) {
    cycle := cycle + 1.U
  }

  val correct_phase = state_is_remd_corr | state_is_quot_corr
  val check_phase = state_is_remd_chck

  //  Start Division
  val dividend = Cat(Fill(xLen+1, lrs1_sign_bit), lrs1_sign_bit, lrs1)
  val divisor = Cat(lrs2_sign_bit, lrs2_sign_bit, lrs2)
  val quot_0cycle = ~(dividend(129) ^ divisor(65))
  val dividend_sft = Cat(dividend(129, 0), quot_0cycle)

  //  Adder
  val adder_op1 = Wire(UInt(66.W))
  val adder_op2 = Wire(UInt(66.W))
  val adder_sub = Wire(Bool())
  val adder_res = Wire(UInt(66.W))

  val part_quot = Reg(UInt(65.W))
  val part_remd_sft = Reg(Bool())
  val part_remd = Reg(UInt(65.W))
  val prev_quot = Mux(cycle_0th, quot_0cycle, part_quot(0))
  val exe_part_remd = Wire(UInt())
  val div_remd = Mux(check_phase, part_remd(64,0),
    Mux(correct_phase, adder_res(64,0), exe_part_remd(129, 65)))
  val div_quot = Mux(check_phase, part_quot(64,0),
    Mux(correct_phase, part_quot(64,0), Cat(exe_part_remd(63, 0), 1.U(1.W))))

  val current_quot = ~(adder_res(65) ^ divisor(65))
  exe_part_remd := Cat(adder_res, Mux(cycle_0th, dividend_sft(64, 0), part_quot(64,0)))
  val exe_part_remd_sft = Cat(exe_part_remd(130,0), current_quot)

  //  Part Remainder
  val part_remd_ena = (state_is_0th & (next_state === s_div)) |
    (state_is_div & ~last_cycle) |
    (state_is_div & last_cycle) |
    state_is_remd_corr

  val part_remd_next = Mux(correct_phase, adder_res(64, 0),
    Mux(state_is_div & last_cycle, div_remd, exe_part_remd_sft))
  when (part_remd_ena) { part_remd_sft := adder_res(64) }
  when (part_remd_ena) { part_remd := part_remd_next }

  //  Part Quot
  val part_quot_ena = (state_is_0th & (next_state === s_div)) |
    (state_is_div && ~last_cycle) |
    (state_is_div && last_cycle) |
    state_is_quot_corr
  val part_quot_next = Mux(correct_phase, adder_res(64,0),
    Mux(state_is_div & last_cycle, div_quot, exe_part_remd_sft(64,0)))
  when (part_quot_ena) { part_quot := part_quot_next }

  val remd_is_zero = ~part_remd.orR
  val remd_is_neg_divs = ~adder_res.orR
  val remd_is_divs = part_remd === divisor(64,0)

  //  Need Correct Valid when
  //  1. Different Sign Bit and Reminder is Not Zero.
  //  2. Adder Result is Zero.
  //  3. Partial Reminder is equal divisor.
  val div_need_corr = ((part_remd(64) ^ dividend(129)) & ~remd_is_zero) | remd_is_neg_divs | remd_is_divs
  val remd_inc_quot_dec = part_remd(64) ^ divisor(65)

  //  Adder
  val div_op1 = Mux(cycle_0th, dividend_sft(130,65), Cat(part_remd_sft, part_remd))
  val div_op2 = divisor
  val div_sub = prev_quot

  val remd_chck_op1 = Cat(part_remd(64), part_remd)
  val remd_chck_op2 = divisor
  val remd_chck_sub = false.B

  val quot_corr_op1 = Cat(part_quot(64), part_quot)
  val quot_corr_op2 = 1.U(66.W)
  val quot_corr_sub = remd_inc_quot_dec

  val remd_corr_op1 = Cat(part_remd(64), part_remd)
  val remd_corr_op2 = divisor
  val remd_corr_sub = ~remd_inc_quot_dec

  adder_op1 := Mux(state_is_0th | state_is_div, div_op1,
    Mux(state_is_quot_corr, quot_corr_op1,
      Mux(state_is_remd_corr, remd_corr_op1, remd_chck_op1)))
  adder_op2 := Mux(state_is_0th | state_is_div, div_op2,
    Mux(state_is_quot_corr, quot_corr_op2,
      Mux(state_is_remd_chck, remd_corr_op2, remd_chck_op2)))
  adder_sub := Mux(state_is_0th | state_is_div, div_sub,
    Mux(state_is_quot_corr, quot_corr_sub,
      Mux(state_is_remd_corr, remd_corr_sub, remd_chck_sub)))
  adder_res := adder_op1 + Mux(adder_sub, ~adder_op2, adder_op2) + Cat(Fill(64, 0.U), adder_sub)

  //  Special result
  /*
   * +-----------------+------------+------------+------------+------------+------------+------------+
   * |    Condition    |  Dividend  |   Divisor  |   DIVU[W]  |  REMU[W]   |  DIV[W]    |   REM[W]   |
   * |                 |            |            |            |            |            |            |
   * +-----------------+------------+------------+------------+------------+------------+------------+
   * |  Division By 0  |     x      |     0      |  2^L - 1   |     x      |     -1     |     x      |
   * |                 |            |            |            |            |            |            |
   * +-----------------+------------+------------+------------+------------+------------+------------+
   * |  Overflow       |  -2^(L-1)  |     -1     |     -      |    -       |   -2^L-1   |     0      |
   * |   (signed only) |            |            |            |            |            |            |
   * +-----------------+------------+------------+------------+------------+------------+------------+
   */
  val div_by_zero = ~lrs2.orR
  val div_by_zero_res_quot = Fill(64, 1.U)
  val div_by_zero_res_remd = dividend(63,0)
  val div_by_zero_res = Mux(is_div, div_by_zero_res_quot, div_by_zero_res_remd)

  val div_ovf = lrs2.andR & (lrs1(63) & ~lrs1(62,0).orR)
  val div_ovf_res_quot = MuxCase(Cat(1.U(1.W), 0.U(63.W)),
    Array(
      (io.req.uop.dw === DW_8) -> Cat(Fill(56, 1.U), 0.U(8.W)),
      (io.req.uop.dw === DW_16) -> Cat(Fill(48, 1.U), 0.U(16.W)),
      (io.req.uop.dw === DW_32) -> Cat(Fill(32, 1.U), 0.U(32.W))
    )
  )
  val div_ovf_res_remd = 0.U(64.W)
  val div_ovf_res = Mux(is_div, div_ovf_res_quot, div_ovf_res_remd)

  val has_special_result = div_by_zero | div_ovf
  val div_special_res = Mux(div_by_zero, div_by_zero_res, div_ovf_res)
  val div_pre_res = Mux(has_special_result, div_special_res,
    Mux(is_div, part_quot(63,0), part_remd(63,0)))
  val div_post_res = MuxCase(div_pre_res,
    Array (
      (io.req.uop.dw === DW_8) -> Cat(Fill(56, ~io.req.uop.usign & div_pre_res(7)), div_pre_res(7,0)),
      (io.req.uop.dw === DW_16) -> Cat(Fill(48, ~io.req.uop.usign & div_pre_res(15)), div_pre_res(15,0)),
      (io.req.uop.dw === DW_32) -> Cat(Fill(32, ~io.req.uop.usign & div_pre_res(31)), div_pre_res(31,0))
    )
  )
  //
  next_state := state
  switch (state) {
    is (s_idle) {
      when (io.req.valid) {
        when (has_special_result) {
          next_state := s_idle
        } .otherwise {
          next_state := s_div
        }
      }
    }

    is (s_div) {
      when (last_cycle) {
        next_state := s_remd_chck
      }
    }

    is (s_remd_chck) {
      when (div_need_corr) {
        next_state := s_quot_corr
      } .otherwise {
        next_state := s_idle
      }
    }

    is (s_quot_corr) {
      next_state := s_remd_corr
    }

    is (s_remd_corr) {
      next_state := s_idle
    }

  }


  //  Division Done Valid when
  //  1. Has Special Result
  //  2. State is Reminder Check and do not correct.
  //  3. State is Reminder Correct.
  val div_done = has_special_result | (state_is_remd_chck & ~div_need_corr) | state_is_remd_corr
  io.resp.valid := div_done
  io.resp.rob_id := io.req.uop.rob_id
  io.resp.data := div_post_res
  io.resp.cause := 0.U
  io.writeback_req.valid := div_done
  io.writeback_req.pntr := io.req.uop.rob_id
  io.writeback_req.data := div_post_res

  //  Long Pipe Stall Valid when
  //  1. Issue Valid and Has Special Result.
  //  2. Current State is Not IDLE.
  io.stall := (io.req.valid & ~has_special_result) | (state =/= s_idle)
}
