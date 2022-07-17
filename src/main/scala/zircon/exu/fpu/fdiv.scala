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
package zircon.exu.fpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import hardfloat._
import zircon.common._
import zircon.issue._

class FDIVDecoder(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val uopc = Input(UInt(UOP_SZ.W))
    val sigs = Output(new FPUCtrlSigs)
  })

  val S = BitPat("b0")
  val D = BitPat("b1")
  val decoder = freechips.rocketchip.rocket.DecodeLogic(io.uopc,
    /* Default */           List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X),
    Array(
      BitPat(UOP_DIV_S)  -> List(X,X,Y,Y,X, X,X,S,S,X,X,X, X,Y,N,Y),
      BitPat(UOP_DIV_D)  -> List(X,X,Y,Y,X, X,X,D,D,X,X,X, X,Y,N,Y),
      BitPat(UOP_SQRT_S) -> List(X,X,Y,N,X, X,X,S,S,X,X,X, X,N,Y,Y),
      BitPat(UOP_SQRT_D) -> List(X,X,Y,N,X, X,X,D,D,X,X,X, X,N,Y,Y)
    ): Array[(BitPat, List[BitPat])])

  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
    s.swap23, s.singleIn, s.singleOut, s.fromint, s.toint, s.fastpipe, s.fma,
    s.div, s.sqrt, s.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FDIVReq(implicit p: Parameters) extends IssueResp {
  val rm = UInt(FPConstants.RM_SZ.W)
}

class FDIV(implicit p: Parameters) extends BaseZirconModule
  with HasFPUParameters
{
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val req = Input(new FDIVReq)
    val resp = Output(new RobExecReq)
  })

  val fdiv_decoder = Module(new FDIVDecoder)
  fdiv_decoder.io.uopc := io.req.uop.uopc
  val ctrl_sigs = fdiv_decoder.io.sigs

  def upconvert(x: UInt) = {
    val s2d = Module(new hardfloat.RecFNToRecFN(inExpWidth = 8, inSigWidth = 24, outExpWidth = 11, outSigWidth = 53))
    s2d.io.in := x
    s2d.io.roundingMode := 0.U
    s2d.io.detectTininess := DontCare
    s2d.io.out
  }

  val in1_upconvert = upconvert(unbox(io.req.uop.lrs1, false.B, Some(FType.S)))
  val in2_upconvert = upconvert(unbox(io.req.uop.lrs2, false.B, Some(FType.S)))

  val S = BitPat("b0")
  val D = BitPat("b1")

  val divsqrt = Module(new DivSqrtRecF64)
  divsqrt.io.inValid := io.req.valid & !io.kill
  divsqrt.io.sqrtOp := fdiv_decoder.io.sigs.sqrt
  divsqrt.io.a := in1_upconvert
  divsqrt.io.b := in2_upconvert
  divsqrt.io.roundingMode := Mux(io.req.uop.dyn_rm, io.req.rm, io.req.uop.rm)
  divsqrt.io.detectTininess := DontCare

  val downvert_d2s = Module(new RecFNToRecFN(inExpWidth = 11, inSigWidth = 53, outExpWidth = 8, outSigWidth = 24))
  downvert_d2s.io.in := sanitizeNaN(divsqrt.io.out, FType.D)
  downvert_d2s.io.roundingMode := io.req.rm
  downvert_d2s.io.detectTininess := DontCare

  val wflags = divsqrt.io.exceptionFlags | Mux(fdiv_decoder.io.sigs.singleIn === S, downvert_d2s.io.exceptionFlags, 0.U)
  io.resp.valid := divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt
  io.resp.rob_id := RegNext(io.req.uop.rob_id)
  io.resp.data := Mux(ctrl_sigs.singleIn === S, box(downvert_d2s.io.out, false.B), box(downvert_d2s.io.in, true.B))
  io.resp.cause := wflags
}