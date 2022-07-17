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
package zircon.issue

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.rob._

class IssueDispatch(
                     plWidth: Int,
                     numIssuePorts: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val reqs = Flipped(Valid(Vec(plWidth, new IssueReadResp)))
    val resps = Valid(Vec(plWidth, new IssueReadResp))
    val writeback_reqs = Input(Vec(numIssuePorts, new IssueWriteBack))
    val disp = Valid(Vec(plWidth, new RobIssueReq))
  })

  //
  io.resps.valid := io.reqs.valid
  for (w <- 0 until plWidth) {
    io.resps.bits(w).valid := io.reqs.bits(w).valid
    io.resps.bits(w).uop := io.reqs.bits(w).uop
    io.resps.bits(w).rsv_id := io.reqs.bits(w).rsv_id

    //  Bypass
    io.resps.bits(w).uop.lrs1 := io.writeback_reqs.foldLeft(io.reqs.bits(w).uop.lrs1) { case (old_data, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs1_map.pntr && io.reqs.bits(w).lrs1_map.busy, req.data, old_data)
    }
    io.resps.bits(w).uop.lrs2 := io.writeback_reqs.foldLeft(io.reqs.bits(w).uop.lrs2) { case (old_data, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs2_map.pntr && io.reqs.bits(w).lrs2_map.busy, req.data, old_data)
    }
    io.resps.bits(w).uop.lrs3 := io.writeback_reqs.foldLeft(io.reqs.bits(w).uop.lrs3){ case (old_data, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs3_map.pntr && io.reqs.bits(w).lrs3_map.busy, req.data, old_data)
    }

    //  Update Mapping
    io.resps.bits(w).lrs1_map := io.reqs.bits(w).lrs1_map
    io.resps.bits(w).lrs1_map.busy := io.writeback_reqs.foldLeft(io.reqs.bits(w).lrs1_map.busy){ case (old_busy, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs1_map.pntr && io.reqs.bits(w).lrs1_map.busy, false.B, old_busy)
    }
    io.resps.bits(w).lrs2_map := io.reqs.bits(w).lrs2_map
    io.resps.bits(w).lrs2_map.busy := io.writeback_reqs.foldLeft(io.reqs.bits(w).lrs2_map.busy){ case (old_busy, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs2_map.pntr && io.reqs.bits(w).lrs2_map.busy, false.B, old_busy)
    }
    io.resps.bits(w).lrs3_map := io.reqs.bits(w).lrs3_map
    io.resps.bits(w).lrs3_map.busy := io.writeback_reqs.foldLeft(io.reqs.bits(w).lrs3_map.busy){ case (old_busy, req) =>
      Mux(req.valid && req.pntr === io.reqs.bits(w).lrs3_map.pntr && io.reqs.bits(w).lrs3_map.busy, false.B, old_busy)
    }
  }

  //  Dispatch
  io.disp.valid := io.reqs.valid
  for (w <- 0 until plWidth) {
    io.disp.bits(w).valid     := io.reqs.bits(w).valid
    io.disp.bits(w).rob_id    := io.reqs.bits(w).uop.rob_id
    io.disp.bits(w).is_ld     := io.reqs.bits(w).uop.is_ld
    io.disp.bits(w).ld_id     := io.reqs.bits(w).uop.ld_id
    io.disp.bits(w).is_st     := io.reqs.bits(w).uop.is_st
    io.disp.bits(w).st_id     := io.reqs.bits(w).uop.st_id
    io.disp.bits(w).uopc      := io.reqs.bits(w).uop.uopc
    io.disp.bits(w).ldst_vld  := io.reqs.bits(w).uop.ldst_vld
    io.disp.bits(w).ldst_type := io.reqs.bits(w).uop.ldst_type
    io.disp.bits(w).ldst_lreg := io.reqs.bits(w).uop.ldst_lreg
    io.disp.bits(w).cause     := io.reqs.bits(w).uop.cause
  }
}