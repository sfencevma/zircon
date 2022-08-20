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

class IssueDispatchResp(implicit p: Parameters) extends BaseZirconBundle {
  val valid     = Bool()
  val rsv_id    = UInt(rsvIdBits.W)
  val uop       = new IssueMicroOp
  val lrs1_map  = new MapTableEntry
  val lrs2_map  = new MapTableEntry
  val lrs3_map  = new MapTableEntry
}

class IssueDispatch(plWidth: Int, numIssuePorts: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val reqs = Flipped(Valid(Vec(plWidth, new IssueReadResp)))
    val resps = Valid(Vec(plWidth, new IssueDispatchResp))
    val writeback_reqs = Input(Vec(numIssuePorts, new IssueWriteBack))
    val disp = Valid(Vec(plWidth, new RobIssueReq))
  })

  //
  val iss_uops = Wire(Vec(plWidth, new IssueMicroOp))

  io.resps.valid := io.reqs.valid
  for (w <- 0 until plWidth) {
    //  Pack
    val uop = io.reqs.bits(w).uop
    iss_uops(w).uopc      := uop.uopc
    iss_uops(w).len       := uop.len
    iss_uops(w).dw        := uop.dw
    iss_uops(w).port      := uop.port
    iss_uops(w).usign     := uop.usign

    iss_uops(w).lrs1_vld  := uop.lrs1_vld
    iss_uops(w).lrs1_type := uop.lrs1_type
    iss_uops(w).lrs1      := uop.lrs1
    iss_uops(w).lrs2_vld  := uop.lrs2_vld
    iss_uops(w).lrs2_type := uop.lrs2_type
    iss_uops(w).lrs2      := uop.lrs2
    iss_uops(w).lrs3_vld  := uop.lrs3_vld
    iss_uops(w).lrs3_type := uop.lrs3_type
    iss_uops(w).lrs3      := uop.lrs3
    iss_uops(w).ldst_vld  := uop.ldst_vld
    iss_uops(w).imm       := uop.imm

    iss_uops(w).dyn_rm    := uop.dyn_rm
    iss_uops(w).rm        := uop.rm

    iss_uops(w).rob_id    := uop.rob_id
    iss_uops(w).is_ld     := uop.is_ld
    iss_uops(w).ld_id     := uop.ld_id
    iss_uops(w).is_st     := uop.is_st
    iss_uops(w).st_id     := uop.st_id
    iss_uops(w).wakeup    := uop.wakeup
    iss_uops(w).mem_cmd   := uop.mem_cmd

    iss_uops(w).addr      := uop.addr
    iss_uops(w).cause     := uop.cause

    iss_uops(w).is_amo    := uop.is_amo
    iss_uops(w).is_csr    := uop.is_csr

    //
    io.resps.bits(w).valid  := io.reqs.bits(w).valid
    io.resps.bits(w).uop    := iss_uops(w)
    io.resps.bits(w).rsv_id := uop.rsv_id

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
  val rob_uops = Wire(Vec(plWidth, new RobMicroOp))
  io.disp.valid := io.reqs.valid
  for (w <- 0 until plWidth) {
    val uop = io.reqs.bits(w).uop
    rob_uops(w).uopc      := uop.uopc
    rob_uops(w).len       := uop.len
    rob_uops(w).port      := uop.port
    rob_uops(w).ldst_vld  := uop.ldst_vld
    rob_uops(w).ldst_type := uop.ldst_type
    rob_uops(w).ldst_lreg := uop.ldst_lreg
    rob_uops(w).csr_addr  := uop.imm(instBits-1, 20)

    rob_uops(w).is_jmp    := uop.is_jmp
    rob_uops(w).is_br     := uop.is_br
    rob_uops(w).is_call   := uop.is_call
    rob_uops(w).is_ret    := uop.is_ret
    rob_uops(w).taken     := io.reqs.bits(w).taken
    rob_uops(w).tg_addr   := io.reqs.bits(w).tg_addr

    rob_uops(w).is_amo    := uop.is_amo
    rob_uops(w).is_csr    := uop.is_csr

    rob_uops(w).rob_id    := uop.rob_id
    rob_uops(w).is_ld     := uop.is_ld
    rob_uops(w).ld_id     := uop.ld_id
    rob_uops(w).is_st     := uop.is_st
    rob_uops(w).st_id     := uop.st_id

    rob_uops(w).addr      := uop.addr
    rob_uops(w).cause     := uop.cause

    //
    io.disp.bits(w).valid := io.reqs.bits(w).valid
    io.disp.bits(w).uop   := rob_uops(w)
  }
}