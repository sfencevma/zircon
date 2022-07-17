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
import zircon.util._

class ALU(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
{
  val io = IO(new Bundle() {
    val req = Input(new IssueResp)
    val resp = Output(new RobExecReq)
    val writeback_req = Output(new IssueWriteBack)
  })

  val lrs1 = io.req.uop.lrs1
  val lrs2 = io.req.uop.lrs2
  val is_sub = io.req.uop.uopc === UOP_SUB
  val uopc = io.req.uop.uopc

  //  ADD, SUB
  val rs2_inv = Mux(is_sub, ~lrs2, lrs2)
  val add = lrs1 + rs2_inv + is_sub

  //  LUI
  val lui = Cat(lrs2(xLen-1,12), 0.U(12.W))

  //  SLT, SLTU
  val slt = Cat(Fill(xLen-1, 0.U), Mux(io.req.uop.usign, lrs1 < lrs2, lrs1.asSInt < lrs2.asSInt))

  //  SLL, SRL, SRA
  val (shamt, shin_r) = {
    val shin_hi_32 = Fill(32, is_sub && lrs1(31))
    val shin_hi = Mux(io.req.uop.lrs1_type === DW_64, lrs1(xLen-1,32), shin_hi_32)
    val shamt = Cat((io.req.uop.lrs1_type === DW_64) && lrs2(5), lrs2(4, 0))
    (shamt, Cat(shin_hi, lrs1(31, 0)))
  }
  val shin = Mux(isOneOf(io.req.uop.uopc, Seq(UOP_SRA, UOP_SRL, UOP_SLL)), shin_r, Reverse(shin_r))
  val shout_r = ((Cat(is_sub & shin(xLen-1), shin)).asSInt >> shamt)(xLen-1, 0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(isOneOf(io.req.uop.uopc, Seq(UOP_SRA, UOP_SRL)), shout_r, 0.U) |
    Mux(isOneOf(io.req.uop.uopc, Seq(UOP_SLL)), shout_l, 0.U)


  //  AND, OR, XOR
  val and = lrs1 & lrs2
  val or  = lrs1 | lrs2
  val xor = lrs1 ^ lrs2
  val logic = Mux(isOneOf(uopc, Seq(UOP_OR, UOP_SET)), or,
    Mux(uopc === UOP_AND, and, xor))

  //  Clear
  val clr = ~lrs1 & lrs2

  //  XChg
  val xchg = lrs1

  //  Set
  val set = or

  //
  val out = Mux(isOneOf(uopc, Seq(UOP_ADD, UOP_SUB)), add,
    Mux(uopc === UOP_XCHG, xchg,
      Mux(uopc === UOP_SLT, slt,
        Mux(uopc === UOP_LUI, lui, logic))))

  io.resp.valid := RegNext(io.req.valid)
  io.resp.rob_id := RegNext(io.req.uop.rob_id)
  io.resp.data := RegNext(Mux(io.req.uop.dw =/= DW_64, Cat(Fill(xLen/2, out(xLen/2) && io.req.uop.usign), out(xLen/2-1,0)), out))
  io.resp.cause := RegNext(io.req.uop.cause)

  io.writeback_req.valid := RegNext(io.req.valid && io.req.uop.ldst_vld)
  io.writeback_req.pntr := io.resp.rob_id
  io.writeback_req.data := io.resp.data
}