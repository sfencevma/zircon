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

class BRU(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val req = Input(new IssueResp)
    val resp = Output(new RobExecReq)
    val writeback_req = Output(new IssueWriteBack)
  })

  val lrs1 = io.req.uop.lrs1
  val lrs2 = io.req.uop.lrs2
  val imm = io.req.uop.lrs3
  val pc = io.req.uop.addr

  val cmp_eq = io.req.uop.uopc === UOP_BEQ
  val cmp_lt = io.req.uop.uopc === UOP_BLT
  val cmp_ne = io.req.uop.uopc === UOP_BNE
  val cmp_ge = io.req.uop.uopc === UOP_BGE

  val lrs1_extend = Cat(Mux(io.req.uop.usign, 0.U(1.W), lrs1(xLen-1)), lrs1).asSInt
  val lrs2_extend = Cat(Mux(io.req.uop.usign, 0.U(1.W), lrs2(xLen-1)), lrs2).asSInt
  val cmp_res = Mux(cmp_eq, lrs1_extend === lrs2_extend,
    Mux(cmp_lt, lrs1_extend< lrs2_extend,
      Mux(cmp_ne, lrs1_extend =/= lrs2_extend, lrs1_extend >= lrs2_extend)))
  val tg_addr = pc + imm

  io.resp.valid := RegNext(io.req.valid)
  io.resp.rob_id := RegNext(io.req.uop.rob_id)
  io.resp.data := RegNext(Cat(cmp_res.asUInt, zeroExtend(tg_addr, xLen-1)))
  io.resp.cause := 0.U
  io.writeback_req := 0.U.asTypeOf(new IssueWriteBack)
}
