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
package zircon.exu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.utils._

class BRU(implicit p: Parameters) extends BaseZirconModule
  with ScalarOpConstants
{
  val io = IO(new Bundle() {
    val req = Input(new IssueResp)
    val resp = Output(new RobExecReq(hasBRU = true))
    val writeback_req = Output(new IssueWriteBack)
  })

  def is_branch = isOneOf(io.req.uop.uopc, Seq(UOP_BNE, UOP_BEQ, UOP_BLT, UOP_BGE))
  def is_jal = isOneOf(io.req.uop.uopc, Seq(UOP_JAL, UOP_JALR))

  val uopc = io.req.uop.uopc
  val usign= io.req.uop.usign
  val lrs1 = io.req.uop.lrs1
  val lrs2 = io.req.uop.lrs2
  val taken = MuxCase(false.B,
    Array(
      (uopc === UOP_BNE)            -> (lrs1 === lrs2),
      (uopc === UOP_BEQ)            -> (lrs1 =/= lrs2),
      ((uopc === UOP_BLT) & usign)  -> (lrs1 < lrs2),
      ((uopc === UOP_BLT) & !usign) -> (lrs1.asSInt < lrs2.asSInt),
      ((uopc === UOP_BGE) & usign)  -> (lrs1 >= lrs2),
      ((uopc === UOP_BGE) & !usign) -> (lrs1.asSInt >= lrs2.asSInt),
      (uopc === UOP_JAL)            -> true.B,
      (uopc === UOP_JALR)           -> true.B
    ))

  val next_addr = io.req.uop.addr + Mux(io.req.uop.len, 2.U, 4.U)
  val tg_addr = Mux(taken, io.req.uop.addr + io.req.uop.imm, next_addr)

  io.resp.valid   := RegNext(io.req.valid)
  io.resp.rob_id  := RegNext(io.req.uop.rob_id)
  io.resp.data    := RegNext(signedExtend(next_addr, xLen))
  io.resp.cause   := 0.U
  io.resp.taken   := RegNext(taken)
  io.resp.tg_addr := RegNext(tg_addr)

  io.writeback_req.valid := RegNext(io.req.valid && io.req.uop.ldst_vld)
  io.writeback_req.pntr  := io.resp.rob_id
  io.writeback_req.data  := io.resp.data
}