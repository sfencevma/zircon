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

class IssueReadResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val uop       = new MicroOp
  val lrs1_map  = new MapTableEntry
  val lrs2_map  = new MapTableEntry
  val lrs3_map  = new MapTableEntry
  val taken     = Bool()
  val tg_addr   = UInt(vaddrBits.W)
}

class IssueReadRobPortIO(implicit p: Parameters) extends BaseZirconBundle {
  val addr  = Input(UInt(robIdBits.W))
  val valid = Output(Bool())
  val data  = Output(UInt(xLen.W))
}


class IssueRead(plWidth: Int, numRegReadPorts: Int)(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val numRobReadPorts: Int = numRegReadPorts

  val io = IO(new Bundle() {
    val kill = Input(Bool())

    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, new RenameStageResp)))
    val resps = Valid(Vec(plWidth, new IssueReadResp))

    //  RegFile to IssueRead
    val reg_read_ports = Flipped(Vec(numRegReadPorts, new RegFileReadPortIO(numLRegs, xLen)))
    val fp_reg_read_ports = Flipped(Vec(numRegReadPorts, new RegFileReadPortIO(numLRegs, xLen)))

    //  Rob to IssueRead
    val rob_read_ports = Flipped(Vec(numRobReadPorts, new IssueReadRobPortIO))

    //  CSR to IssueRead
    val csr_read_port = Flipped(new CsrReadPortIO)
  })

  //  Read from RegFiles
  val reg_reqs = io.reqs.bits.map(_.uop.lrs1_lreg) ++ io.reqs.bits.map(_.uop.lrs2_lreg) ++ io.reqs.bits.map(_.uop.lrs3_lreg)
  io.reg_read_ports zip reg_reqs map { case (p, r) => p.addr := r }
  io.fp_reg_read_ports zip reg_reqs map { case (p, r) => p.addr := r }
  val reg_datas_group = io.reg_read_ports.map(_.data).grouped(plWidth).toList
  val fp_reg_datas_group = io.fp_reg_read_ports.map(_.data).grouped(plWidth).toList

  val lrs1_reg_datas = reg_datas_group(0)
  val lrs2_reg_datas = reg_datas_group(1)
  val lrs3_reg_datas = reg_datas_group(2)
  val lrs1_fp_reg_datas = fp_reg_datas_group(0)
  val lrs2_fp_reg_datas = fp_reg_datas_group(1)
  val lrs3_fp_reg_datas = fp_reg_datas_group(2)

  //  Read from Rob
  val rob_reqs = io.reqs.bits.map(_.lrs1_map.pntr) ++ io.reqs.bits.map(_.lrs2_map.pntr) ++ io.reqs.bits.map(_.lrs3_map.pntr)
  io.rob_read_ports zip rob_reqs map { case (p, r) => p.addr := r }
  val rob_valids_group = io.rob_read_ports.map(_.valid).grouped(plWidth).toList
  val rob_datas_group = io.rob_read_ports.map(_.data).grouped(plWidth).toList
  val lrs1_rob_valids= rob_valids_group(0)
  val lrs1_rob_datas = rob_datas_group(0)
  val lrs2_rob_valids= rob_valids_group(1)
  val lrs2_rob_datas = rob_datas_group(1)
  val lrs3_rob_valids= rob_valids_group(2)
  val lrs3_rob_datas = rob_datas_group(2)

  //  Read from CSR
  val csr_req = io.reqs.bits(0).uop.imm(csrAddrBits-1,0)
  io.csr_read_port.addr := csr_req
  val csr_data = io.csr_read_port.data

  //  Select Data
  val lrs1_sel_datas = Wire(Vec(plWidth, UInt(xLen.W)))
  val lrs2_sel_datas = Wire(Vec(plWidth, UInt(xLen.W)))
  val lrs3_sel_datas = Wire(Vec(plWidth, UInt(xLen.W)))

  for (w <- 0 until plWidth) {
    //  Which Data?
    //  1.  When Logical Register is Valid, Busy and Not Ready -> ROB ID
    //  2.  When Logical Register is Valid, Busy and Ready -> ROB Data
    //  3.  Otherwise -> Regfile Data
    lrs1_sel_datas(w) := MuxCase(Mux(io.reqs.bits(w).uop.uopc === RT_FP, lrs1_fp_reg_datas(w), lrs1_reg_datas(w)), Array(
      (io.reqs.bits(w).valid & io.reqs.bits(w).lrs1_map.busy & !lrs1_rob_valids(w))   -> zeroExtend(io.reqs.bits(w).uop.rob_id, xLen),
      (io.reqs.bits(w).valid & io.reqs.bits(w).lrs2_map.busy & lrs1_rob_valids(w))    -> lrs1_rob_datas(w)
    ))

    if (w == 0) {
      lrs2_sel_datas(w) := MuxCase(Mux(io.reqs.bits(w).uop.lrs2_type === RT_CSR, csr_data,
        Mux(io.reqs.bits(w).uop.uopc === RT_FP, lrs2_fp_reg_datas(w), lrs2_reg_datas(w))), Array(
        (io.reqs.bits(w).valid & io.reqs.bits(w).lrs2_map.busy & !lrs2_rob_valids(w))   -> zeroExtend(io.reqs.bits(w).uop.rob_id, xLen),
        (io.reqs.bits(w).valid & io.reqs.bits(w).lrs2_map.busy & lrs2_rob_valids(w))    -> lrs2_rob_valids(w)
      ))
    } else {
      lrs2_sel_datas(w) := MuxCase(Mux(io.reqs.bits(w).uop.uopc === RT_FP, lrs2_fp_reg_datas(w), lrs2_reg_datas(w)), Array(
        (io.reqs.bits(w).valid & io.reqs.bits(w).lrs2_map.busy & !lrs2_rob_valids(w))   -> zeroExtend(io.reqs.bits(w).uop.rob_id, xLen),
        (io.reqs.bits(w).valid & io.reqs.bits(w).lrs2_map.busy & lrs2_rob_valids(w))    -> lrs2_rob_datas(w)
      ))
    }

    lrs3_sel_datas(w) := MuxCase(Mux(io.reqs.bits(w).uop.uopc === RT_FP, lrs3_fp_reg_datas(w), lrs3_reg_datas(w)), Array(
      (io.reqs.bits(w).valid & io.reqs.bits(w).lrs3_map.busy & !lrs3_rob_valids(w))   -> zeroExtend(io.reqs.bits(w).uop.rob_id, xLen),
      (io.reqs.bits(w).valid & io.reqs.bits(w).lrs3_map.busy & lrs3_rob_valids(w))    -> lrs3_rob_datas(w)
    ))
  }

  //
  val lrs1_from_rob = io.reqs.bits zip lrs1_rob_valids map { case (req, v) => req.uop.lrs1_vld && req.lrs1_map.busy && v }
  val lrs2_from_rob = io.reqs.bits zip lrs2_rob_valids map { case (req, v) => req.uop.lrs2_vld && req.lrs2_map.busy && v }
  val lrs3_from_rob = io.reqs.bits zip lrs3_rob_valids map { case (req, v) => req.uop.lrs3_vld && req.lrs3_map.busy && v }

  val resp_valid = Reg(Bool())
  val resp = Reg(Vec(plWidth, new IssueReadResp))
  for (w <- 0 until plWidth) {
    resp(w).valid := io.reqs.bits(w).valid
    resp(w).uop := io.reqs.bits(w).uop
    resp(w).taken := io.reqs.bits(w).taken
    resp(w).tg_addr := io.reqs.bits(w).tg_addr

    resp(w).uop.lrs1 := MuxCase(lrs1_sel_datas(w), Array(
      (io.reqs.bits(w).uop.lrs1_type === RT_IMM) -> signedExtend(io.reqs.bits(w).uop.imm, xLen),
      (io.reqs.bits(w).uop.lrs1_type === RT_PC) -> signedExtend(io.reqs.bits(w).uop.addr, xLen)
    ))
    resp(w).uop.lrs2 := MuxCase(lrs2_sel_datas(w), Array(
      (io.reqs.bits(w).uop.lrs2_type === RT_IMM) -> signedExtend(io.reqs.bits(w).uop.imm, xLen),
      (io.reqs.bits(w).uop.lrs2_type === RT_PC) -> signedExtend(io.reqs.bits(w).uop.addr, xLen)
    ))
    resp(w).uop.lrs3 := MuxCase(lrs3_sel_datas(w), Array(
      (io.reqs.bits(w).uop.lrs3_type === RT_IMM) -> signedExtend(io.reqs.bits(w).uop.imm, xLen),
      (io.reqs.bits(w).uop.lrs3_type === RT_PC) -> signedExtend(io.reqs.bits(w).uop.addr, xLen)
    ))

    //  Update Mapping
    resp(w).lrs1_map := io.reqs.bits(w).lrs1_map
    resp(w).lrs1_map.busy := Mux(lrs1_from_rob(w) || !io.reqs.bits(w).uop.lrs1_vld, false.B, io.reqs.bits(w).lrs1_map.busy)

    resp(w).lrs2_map := io.reqs.bits(w).lrs2_map
    resp(w).lrs2_map.busy := Mux(lrs2_from_rob(w) || !io.reqs.bits(w).uop.lrs2_vld, false.B, io.reqs.bits(w).lrs2_map.busy)

    resp(w).lrs3_map := io.reqs.bits(w).lrs3_map
    resp(w).lrs3_map.busy := Mux(lrs3_from_rob(w) || !io.reqs.bits(w).uop.lrs3_vld, false.B, io.reqs.bits(w).lrs3_map.busy)
  }

  when (io.kill) {
    resp_valid := false.B
  }  .otherwise {
    resp_valid := io.reqs.valid
  }

  io.resps.valid := resp_valid
  io.resps.bits := resp
}

