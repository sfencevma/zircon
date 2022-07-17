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

class MUL(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val req = Input(new IssueResp)
    val resp = Output(new RobExecReq)
    val writeback_req = Output(new IssueWriteBack)
  })

  val lrs1 = io.req.uop.lrs1
  val lrs2 = io.req.uop.lrs2

  val is_mulsu = (io.req.uop.uopc === UOP_MULHSU)
  val sel_hi = isOneOf(io.req.uop.uopc, Seq(UOP_MULH, UOP_MULHSU))
  val cmp_half = isOneOf(io.req.uop.uopc, Seq(UOP_MULHSU, UOP_MULH))
  val lrs1_signed = is_mulsu | !io.req.uop.usign
  val lrs2_signed = !io.req.uop.usign
  val lrs1_extend = Cat(lrs1_signed & lrs1(xLen-1), lrs1)
  val lrs2_extend = Cat(lrs2_signed & lrs2(xLen-1), lrs2)
  val prod = lrs1.asSInt * lrs2.asSInt

  val mul_res = Mux(sel_hi, prod(2*xLen-1, xLen), Mux(cmp_half, Cat(Fill(xLen/2, prod(31)), prod(31, 0)), prod(xLen-1,0)))


  io.resp.valid := RegNext(io.req.valid)
  io.resp.rob_id := RegNext(io.req.uop.rob_id)
  io.resp.data := RegNext(mul_res)
  io.resp.cause := 0.U // RegNext(io.req.uop.cause)

  io.writeback_req.valid := RegNext(io.req.valid && io.req.uop.ldst_vld)
  io.writeback_req.pntr := io.resp.rob_id
  io.writeback_req.data := io.resp.data
}