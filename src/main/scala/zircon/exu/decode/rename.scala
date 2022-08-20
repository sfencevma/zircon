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

class RenameReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val rob_id    = UInt(robIdBits.W)
  val lrs1_lreg = UInt(lregSz.W)
  val lrs2_lreg = UInt(lregSz.W)
  val lrs3_lreg = UInt(lregSz.W)
  val ldst_vld  = Bool()
  val ldst_lreg = UInt(lregSz.W)
}


class Rename(plWidth: Int, numEntries: Int, float: Boolean)(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle(){
    val stall = Input(Bool())
    val kill = Input(Bool())
    val reqs = Flipped(Valid(Vec(plWidth, new RenameReq)))
    val resps = Output(Vec(plWidth, new MapResp))
    val dealloc = Flipped(Valid(Vec(plWidth, new MapCommitReq)))
  })

  val map_table = Module(new MapTable(plWidth, numEntries, float))
  //  Maptable Requires
  map_table.io.flush := io.kill
  map_table.io.map_reqs.valid := io.reqs.valid && !io.stall
  for (w <- 0 until plWidth) {
    map_table.io.map_reqs.bits(w).valid     := io.reqs.bits(w).valid
    map_table.io.map_reqs.bits(w).rob_id    := io.reqs.bits(w).rob_id
    map_table.io.map_reqs.bits(w).lrs1_lreg := io.reqs.bits(w).lrs1_lreg
    map_table.io.map_reqs.bits(w).lrs2_lreg := io.reqs.bits(w).lrs2_lreg
    map_table.io.map_reqs.bits(w).lrs3_lreg := io.reqs.bits(w).lrs3_lreg
    map_table.io.map_reqs.bits(w).ldst_vld  := io.reqs.bits(w).ldst_vld
    map_table.io.map_reqs.bits(w).ldst_lreg := io.reqs.bits(w).ldst_lreg
  }

  //  Maptable Outputs
  for (w <- 0 until plWidth) {
    io.resps(w) := map_table.io.map_resps(w)
    // io.resps(w).lrs1_map  := map_table.io.map_resps(w).lrs1_map
    // io.resps(w).lrs2_map  := map_table.io.map_resps(w).lrs2_map
    // io.resps(w).lrs3_map  := map_table.io.map_resps(w).lrs3_map
  }
  //
  map_table.io.commit_reqs := io.dealloc
}

class RenameStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val uop       = new MicroOp
  val lrs1_map  = new MapTableEntry
  val lrs2_map  = new MapTableEntry
  val lrs3_map  = new MapTableEntry
  val taken     = Bool()
  val tg_addr   = UInt(vaddrBits.W)
}

class RenameStageCommit(implicit p: Parameters) extends BaseZirconBundle with ScalarOpConstants {
  val valid     = Bool()
  val rob_id    = UInt(robIdBits.W)
  val ldst_type = UInt(RT_SZ.W)
  val ldst_lreg = UInt(lregSz.W)
}

class RenameStage(plWidth: Int)(implicit p: Parameters) extends BaseZirconModule with ScalarOpConstants {
  val io = IO(new Bundle() {
    val kill = Input(Bool())
    val stall = Input(Bool())
    val reqs = Flipped(Valid(Vec(plWidth, new DecodeResp)))
    val resps = Valid(Vec(plWidth, new RenameStageResp))
    val commit_reqs = Flipped(Valid(Vec(plWidth, new RenameStageCommit)))
  })

  val rename_stage = Module(new Rename(plWidth, numLRegs, false))
  val fp_rename_stage = Module(new Rename(plWidth, numLRegs, true))

  //
  rename_stage.io.kill := io.kill
  rename_stage.io.stall := io.stall
  fp_rename_stage.io.kill := io.kill
  fp_rename_stage.io.stall := io.stall

  //  Require
  val reqs = Wire(Vec(plWidth, new RenameReq))
  val fp_reqs = Wire(Vec(plWidth, new RenameReq))

  for (w <- 0 until plWidth) {
    reqs(w).valid     := io.reqs.bits(w).valid
    reqs(w).rob_id    := io.reqs.bits(w).uop.rob_id
    reqs(w).lrs1_lreg := io.reqs.bits(w).uop.lrs1_lreg
    reqs(w).lrs2_lreg := io.reqs.bits(w).uop.lrs2_lreg
    reqs(w).lrs3_lreg := io.reqs.bits(w).uop.lrs3_lreg
    reqs(w).ldst_vld  := io.reqs.bits(w).uop.ldst_vld && (io.reqs.bits(w).uop.ldst_type === RT_FIX)
    reqs(w).ldst_lreg := io.reqs.bits(w).uop.ldst_lreg

    fp_reqs(w).valid     := io.reqs.bits(w).valid
    fp_reqs(w).rob_id    := io.reqs.bits(w).uop.rob_id
    fp_reqs(w).lrs1_lreg := io.reqs.bits(w).uop.lrs1_lreg
    fp_reqs(w).lrs2_lreg := io.reqs.bits(w).uop.lrs2_lreg
    fp_reqs(w).lrs3_lreg := io.reqs.bits(w).uop.lrs3_lreg
    fp_reqs(w).ldst_vld  := io.reqs.bits(w).uop.ldst_vld && (io.reqs.bits(w).uop.ldst_type === RT_FP)
    fp_reqs(w).ldst_lreg := io.reqs.bits(w).uop.ldst_lreg
  }

  rename_stage.io.reqs.valid := io.reqs.valid
  rename_stage.io.reqs.bits := reqs
  fp_rename_stage.io.reqs.valid := io.reqs.valid
  fp_rename_stage.io.reqs.bits := fp_reqs

  //
  val resps = Reg(Valid(Vec(plWidth, new RenameStageResp)))
  for (w <- 0 until plWidth) {
    when (!io.stall) {
      resps.bits(w).valid := io.reqs.bits(w).valid
      resps.bits(w).uop   := io.reqs.bits(w).uop
      resps.bits(w).taken := io.reqs.bits(w).taken
      resps.bits(w).tg_addr := io.reqs.bits(w).tg_addr

      resps.bits(w).lrs1_map := Mux(io.reqs.bits(w).uop.lrs1_type === RT_FIX,
        rename_stage.io.resps(w).lrs1_map, fp_rename_stage.io.resps(w).lrs1_map)
      resps.bits(w).lrs2_map := Mux(io.reqs.bits(w).uop.lrs2_type === RT_FIX,
        rename_stage.io.resps(w).lrs2_map, fp_rename_stage.io.resps(w).lrs2_map)
      resps.bits(w).lrs3_map := Mux(io.reqs.bits(w).uop.lrs3_type === RT_FIX,
        rename_stage.io.resps(w).lrs3_map, fp_rename_stage.io.resps(w).lrs3_map)
    }
  }

  when (io.kill) {
    resps.valid := false.B
  } .elsewhen (io.stall) {
    resps.valid := resps.valid
  } .otherwise {
    resps.valid := io.reqs.valid
  }

  io.resps <> resps


  //  Update
  val commit_reqs = Wire(Vec(plWidth, new MapCommitReq()))
  val fp_commit_reqs = Wire(Vec(plWidth, new MapCommitReq()))

  for (w <- 0 until plWidth) {
    commit_reqs(w).valid := io.commit_reqs.bits(w).valid && (io.commit_reqs.bits(w).ldst_type === RT_FIX)
    commit_reqs(w).rob_id := io.commit_reqs.bits(w).rob_id
    commit_reqs(w).ldst_lreg := io.commit_reqs.bits(w).ldst_lreg

    fp_commit_reqs(w).valid := io.commit_reqs.bits(w).valid && (io.commit_reqs.bits(w).ldst_type === RT_FP)
    fp_commit_reqs(w).rob_id := io.commit_reqs.bits(w).rob_id
    fp_commit_reqs(w).ldst_lreg := io.commit_reqs.bits(w).ldst_lreg
  }

  rename_stage.io.dealloc.valid := io.commit_reqs.valid
  rename_stage.io.dealloc.bits := commit_reqs
  fp_rename_stage.io.dealloc.valid := io.commit_reqs.valid
  fp_rename_stage.io.dealloc.bits := fp_commit_reqs
}