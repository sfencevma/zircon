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

import Chisel.Bundle
import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import zircon.common._
import zircon.issue.IssueResp

class FPUDecoder(implicit p: Parameters) extends BaseZirconModule
  with HasFPUParameters
  with ScalarOpConstants
{
  val io = IO(new Bundle(){
    val uopc = Input(UInt(UOP_SZ.W))
    val sigs = Output(new FPUCtrlSigs)
  })

  // val S = floatTypes.indexOf(FType.S).asUInt
  // val D = floatTypes.indexOf(FType.D).asUInt
  val S = BitPat("b0")
  val D = BitPat("b1")
  val default: List[BitPat] = List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)
  val f_table: Array[(BitPat, List[BitPat])] =
    Array (
      BitPat(UOP_CLASS_S) -> List(X,X,Y,N,N, N,X,S,S,N,Y,N, N,N,N,N),
      BitPat(UOP_MV_S_X)  -> List(X,X,N,N,N, X,X,S,D,Y,N,N, N,N,N,N),
      BitPat(UOP_MV_X_S)  -> List(X,X,Y,N,N, N,X,D,S,N,Y,N, N,N,N,N),
      BitPat(UOP_CVT_S_X) -> List(X,X,N,N,N, X,X,S,S,Y,N,N, N,N,N,Y),
      BitPat(UOP_CVT_X_S) -> List(X,X,Y,N,N, N,X,S,S,N,Y,N, N,N,N,Y),
      BitPat(UOP_EQ_S)    -> List(X,X,Y,Y,N, N,N,S,S,N,Y,N, N,N,N,Y),
      BitPat(UOP_LT_S)    -> List(X,X,Y,Y,N, N,N,S,S,N,Y,N, N,N,N,Y),
      BitPat(UOP_LE_S)    -> List(X,X,Y,Y,N, N,N,S,S,N,Y,N, N,N,N,Y),
      BitPat(UOP_SGNJ_S)  -> List(X,X,Y,Y,N, N,N,S,S,N,N,Y, N,N,N,N),
      BitPat(UOP_SGNJN_S) -> List(X,X,Y,Y,N, N,N,S,S,N,N,Y, N,N,N,N),
      BitPat(UOP_SGNJX_S) -> List(X,X,Y,Y,N, N,N,S,S,N,N,Y, N,N,N,N),
      BitPat(UOP_MAX_S)   -> List(X,X,Y,Y,N, N,N,S,S,N,N,Y, N,N,N,Y),
      BitPat(UOP_MIN_S)   -> List(X,X,Y,Y,N, N,N,S,S,N,N,Y, N,N,N,Y),
      BitPat(UOP_ADD_S)   -> List(X,X,Y,Y,N, N,Y,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_SUB_S)   -> List(X,X,Y,Y,N, N,Y,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_MUL_S)   -> List(X,X,Y,Y,N, N,N,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_MADD_S)  -> List(X,X,Y,Y,Y, N,N,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_MSUB_S)  -> List(X,X,Y,Y,Y, N,N,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_NMADD_S) -> List(X,X,Y,Y,Y, N,N,S,S,N,N,N, Y,N,N,Y),
      BitPat(UOP_NMSUB_S) -> List(X,X,Y,Y,Y, N,N,S,S,N,N,N, Y,N,N,Y)
    )
  val d_table: Array[(BitPat, List[BitPat])] =
    Array (
      BitPat(UOP_CLASS_D) -> List(X,X,Y,N,N, N,X,D,D,N,Y,N, N,N,N,N),
      BitPat(UOP_MV_D_X)  -> List(X,X,N,N,N, X,X,D,D,Y,N,N, N,N,N,N),
      BitPat(UOP_MV_X_D)  -> List(X,X,Y,N,N, N,X,D,D,N,Y,N, N,N,N,N),
      BitPat(UOP_CVT_S_D) -> List(X,X,Y,N,N, N,X,D,S,N,N,Y, N,N,N,Y),
      BitPat(UOP_CVT_D_S) -> List(X,X,Y,N,N, N,X,S,D,N,N,Y, N,N,N,Y),
      BitPat(UOP_CVT_D_X) -> List(X,X,N,N,N, X,X,D,D,Y,N,N, N,N,N,Y),
      BitPat(UOP_CVT_X_D) -> List(X,X,Y,N,N, N,X,D,D,N,Y,N, N,N,N,Y),
      BitPat(UOP_EQ_D)    -> List(X,X,Y,Y,N, N,N,D,D,N,Y,N, N,N,N,Y),
      BitPat(UOP_LE_D)    -> List(X,X,Y,Y,N, N,N,D,D,N,Y,N, N,N,N,Y),
      BitPat(UOP_LT_D)    -> List(X,X,Y,Y,N, N,N,D,D,N,Y,N, N,N,N,Y),
      BitPat(UOP_SGNJ_D)  -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,N),
      BitPat(UOP_SGNJN_D) -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,N),
      BitPat(UOP_SGNJX_D) -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,N),
      BitPat(UOP_MAX_D)   -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,Y),
      BitPat(UOP_MIN_D)   -> List(X,X,Y,Y,N, N,N,D,D,N,N,Y, N,N,N,Y),
      BitPat(UOP_ADD_D)   -> List(X,X,Y,Y,N, N,Y,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_SUB_D)   -> List(X,X,Y,Y,N, N,Y,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_MUL_D)   -> List(X,X,Y,Y,N, N,N,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_MADD_D)  -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_MSUB_D)  -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_NMADD_D) -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y),
      BitPat(UOP_NMSUB_D) -> List(X,X,Y,Y,Y, N,N,D,D,N,N,N, Y,N,N,Y)
    )

  val insns = f_table ++ d_table
  val decoder = DecodeLogic(io.uopc, default, insns)
  val sigs = Seq(io.sigs.ldst, io.sigs.wen, io.sigs.ren1, io.sigs.ren2, io.sigs.ren3, io.sigs.swap12,
    io.sigs.swap23, io.sigs.singleIn, io.sigs.singleOut, io.sigs.fromint, io.sigs.toint, io.sigs.fastpipe, io.sigs.fma,
    io.sigs.div, io.sigs.sqrt, io.sigs.wflags)
  sigs zip decoder map {case(s,d) => s := d}
}

class FMADecoder extends Module with ScalarOpConstants {
  val io = IO(new Bundle() {
    val uopc = Input(UInt(UOP_SZ.W))
    val cmd = Output(UInt(2.W))
  })

  val default: List[BitPat] = List(BitPat("b??"))
  val table: Array[(BitPat, List[BitPat])] =
    Array(
      BitPat(UOP_ADD_S)   -> List(BitPat("b00")),
      BitPat(UOP_SUB_S)   -> List(BitPat("b01")),
      BitPat(UOP_MUL_S)   -> List(BitPat("b00")),
      BitPat(UOP_MADD_S)  -> List(BitPat("b00")),
      BitPat(UOP_MSUB_S)  -> List(BitPat("b01")),
      BitPat(UOP_NMADD_S) -> List(BitPat("b11")),
      BitPat(UOP_NMSUB_S) -> List(BitPat("b10")),
      BitPat(UOP_ADD_D)   -> List(BitPat("b00")),
      BitPat(UOP_SUB_D)   -> List(BitPat("b01")),
      BitPat(UOP_MUL_D)   -> List(BitPat("b00")),
      BitPat(UOP_MADD_D)  -> List(BitPat("b00")),
      BitPat(UOP_MSUB_D)  -> List(BitPat("b01")),
      BitPat(UOP_NMADD_D) -> List(BitPat("b11")),
      BitPat(UOP_NMSUB_D) -> List(BitPat("b10"))
    )

  val decoder = DecodeLogic(io.uopc, default, table)

  val (cmd: UInt) :: Nil = decoder
  io.cmd := cmd
}

class FPUReq(implicit p: Parameters) extends IssueResp {
  val frm = UInt(FPConstants.RM_SZ.W)
}

class FPU(implicit p: Parameters) extends BaseZirconModule
  with HasFPUParameters
{
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val req = Input(new FPUReq)
    val rm = Input(UInt(3.W))
    val resp = Output(new RobExecReq)
    val stall = Output(Bool())
  })

  val S = BitPat("b0")
  val D = BitPat("b1")
  val fpu_latency = 4
  val fp_decoder = Module(new FPUDecoder)
  fp_decoder.io.uopc := io.req.uop.uopc
  val fp_ctrl = fp_decoder.io.sigs
  val fp_rm = Mux(io.req.uop.dyn_rm, io.req.frm, io.req.uop.rm)

  val lrs1_data = io.req.uop.lrs1
  val lrs2_data = io.req.uop.lrs2
  val lrs3_data = io.req.uop.lrs3
  def fpInput(minT: Option[FType]): FPInput = {
    val req = Wire(new FPInput)
    val tag = floatTypes.indexOf(fp_ctrl.singleIn).asUInt
    req <> fp_ctrl
    req.rm := fp_rm
    req.in1 := unbox(lrs1_data, tag, minT)
    req.in2 := unbox(lrs2_data, tag, minT)
    req.in3 := unbox(lrs3_data, tag, minT)
    when (fp_ctrl.swap23) { req.in3 := req.in2 }
    req.typ := io.req.uop.lrs2_lreg(1, 0)

    val fma_decoder = Module(new FMADecoder)
    fma_decoder.io.uopc := io.req.uop.uopc
    req.fmaCmd := fma_decoder.io.cmd
    req
  }

  val dfma = Module(new FPUFMAPipe(latency = fpu_latency, t = FType.D))
  dfma.io.in.valid := io.req.valid && fp_ctrl.fma && (fp_ctrl.singleOut === D)
  dfma.io.in.bits := fpInput(Some(dfma.t))

  val sfma = Module(new FPUFMAPipe(latency = fpu_latency, t = FType.S))
  sfma.io.in.valid := io.req.valid && fp_ctrl.fma && (fp_ctrl.singleOut === S)
  sfma.io.in.bits := fpInput(Some(sfma.t))

  val fpiu = Module(new FPToInt)
  fpiu.io.in.valid := io.req.valid && (fp_ctrl.toint || (fp_ctrl.fastpipe && fp_ctrl.wflags))
  fpiu.io.in.bits := fpInput(None)
  val fpiu_out = Pipe(RegNext(fpiu.io.in.valid && !fp_ctrl.fastpipe),
    fpiu.io.out.bits, fpu_latency-1)
  val fpiu_result  = Wire(new FPResult)
  fpiu_result.data := fpiu_out.bits.toint
  fpiu_result.exc  := fpiu_out.bits.exc

  val fpmu = Module(new FPToFP(fpu_latency))
  fpmu.io.in.valid := io.req.valid && fp_ctrl .fastpipe
  fpmu.io.in.bits := fpiu.io.in.bits
  fpmu.io.lt := fpiu.io.out.bits.lt
  val fpmu_double = Pipe(io.req.valid && fp_ctrl.fastpipe, fp_ctrl.singleOut === D, fpu_latency).bits

  io.resp.valid := fpiu_out.valid || fpmu.io.out.valid || sfma.io.out.valid || dfma.io.out.valid
  val fpu_out_data =
    Mux(dfma.io.out.valid, box(dfma.io.out.bits.data, true.B),
      Mux(sfma.io.out.valid, box(sfma.io.out.bits.data, false.B),
        Mux(fpiu_out.valid,    fpiu_result.data,
          box(fpmu.io.out.bits.data, fpmu_double))))

  val fpu_out_exc =
    Mux(dfma.io.out.valid, dfma.io.out.bits.exc,
      Mux(sfma.io.out.valid, sfma.io.out.bits.exc,
        Mux(fpiu_out.valid,    fpiu_result.exc,
          fpmu.io.out.bits.exc)))
  io.resp.rob_id := RegNext(io.req.uop.rob_id) // Pipe(RegNext(io.req.uop.rob_id), fpu_latency)
  io.resp.cause := fpu_out_exc
}