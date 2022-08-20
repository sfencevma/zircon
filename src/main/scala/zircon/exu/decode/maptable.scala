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

class MapTableEntry(implicit p: Parameters) extends BaseZirconBundle {
  val busy = Bool()
  val pntr = UInt(robIdBits.W)
}

class MapReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val rob_id    = UInt(robIdBits.W)
  val lrs1_lreg = UInt(lregSz.W)
  val lrs2_lreg = UInt(lregSz.W)
  val lrs3_lreg = UInt(lregSz.W)
  val ldst_vld  = Bool()
  val ldst_lreg = UInt(lregSz.W)
}

class MapResp(implicit p: Parameters) extends BaseZirconBundle {
  val lrs1_map = new MapTableEntry
  val lrs2_map = new MapTableEntry
  val lrs3_map = new MapTableEntry
}

class MapCommitReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid      = Bool()
  val rob_id     = UInt(robIdBits.W)
  val ldst_lreg  = UInt(lregSz.W)
}

class MapRollBackReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val ldst_lreg = UInt(lregSz.W)
}


class MapTable(plWidth: Int, numEntries: Int, float: Boolean)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val map_reqs = Flipped(Valid(Vec(plWidth, new MapReq)))
    val map_resps = Output(Vec(plWidth, new MapResp))
    val commit_reqs = Flipped(Valid(Vec(plWidth, new MapCommitReq)))
  })

  val map_table = RegInit(VecInit(Seq.fill(numEntries){0.U.asTypeOf(new MapTableEntry)}))

  //  Remap
  val remap_table = Wire(Vec(plWidth+1, Vec(numEntries, new MapTableEntry)))
  val commit_maps = io.commit_reqs.bits.map(req => {
    val m = Wire(new MapTableEntry)
    m.busy := false.B
    m.pntr := map_table(req.ldst_lreg).pntr
    m
  })
  val req_maps = io.map_reqs.bits.map(req => {
    val m = Wire(new MapTableEntry)
    m.busy := true.B
    m.pntr := req.rob_id
    m
  })
  val remap_reqs = req_maps ++ commit_maps
  val cmt_remap_ldsts_oh = io.commit_reqs.bits.map(cmt =>
    UIntToOH(cmt.ldst_lreg) &
      Fill(numEntries, io.commit_reqs.valid && cmt.valid) &
      (map_table(cmt.ldst_lreg).pntr === cmt.rob_id))
  val req_remap_ldsts_oh = io.map_reqs.bits.map(req => UIntToOH(req.ldst_lreg) & Fill(numEntries, io.map_reqs.valid && req.valid && req.ldst_vld))

  for (i <- 0 until numEntries) {
    if (i == 0 && !float) {
      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := 0.U.asTypeOf(new MapTableEntry)
      }
    } else {
      val remapped_row = ((req_remap_ldsts_oh.map(ldst => ldst(i)) ++ cmt_remap_ldsts_oh.map(ldst => ldst(i))) zip remap_reqs)
        .scanLeft(map_table(i)) { case (old_map, (ldst, new_map)) => Mux(ldst, new_map, old_map)}

      for (j <- 0 until plWidth+1) {
        remap_table(j)(i) := remapped_row(j)
      }
    }
  }

  when (io.flush) {
    map_table.foreach(_ := 0.U.asTypeOf(new MapTableEntry))
  } otherwise {
    map_table := remap_table(plWidth)
  }

  //  Select Map Bypass Valid when
  //  1.  Register Valid.
  //  2.  Register Match.
  val new_mapping = remap_table(plWidth)

  def gen_entry(busy: Bool, pntr: UInt) = {
    val entry = Wire(new MapTableEntry)
    entry.busy := busy
    entry.pntr := pntr
    entry
  }

  for (w <- 0 until plWidth) {
    //  rs1
    val lrs1_map = new_mapping(io.map_reqs.bits(w).lrs1_lreg)
    io.map_resps(w).lrs1_map := (0 until w).foldLeft(lrs1_map) ((m, k) =>
      Mux(io.map_reqs.bits(k).valid && io.map_reqs.bits(k).ldst_vld && (io.map_reqs.bits(k).ldst_lreg === io.map_reqs.bits(w).lrs1_lreg),
        gen_entry(true.B, io.map_reqs.bits(k).rob_id), m))

    //  rs2
    val lrs2_map = new_mapping(io.map_reqs.bits(w).lrs2_lreg)
    io.map_resps(w).lrs2_map := (0 until w).foldLeft(lrs2_map) ((m, k) =>
      Mux(io.map_reqs.bits(k).valid && io.map_reqs.bits(k).ldst_vld && (io.map_reqs.bits(k).ldst_lreg === io.map_reqs.bits(w).lrs2_lreg),
        gen_entry(true.B, io.map_reqs.bits(k).rob_id), m))

    //  rs3
    val lrs3_map = new_mapping(io.map_reqs.bits(w).lrs3_lreg)
    io.map_resps(w).lrs3_map := (0 until w).foldLeft(lrs3_map) ((m, k) =>
      Mux(io.map_reqs.bits(k).valid && io.map_reqs.bits(k).ldst_vld && (io.map_reqs.bits(k).ldst_lreg === io.map_reqs.bits(w).lrs3_lreg),
        gen_entry(true.B, io.map_reqs.bits(k).rob_id), m))

  }

}