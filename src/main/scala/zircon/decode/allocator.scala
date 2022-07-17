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
package zircon.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.issue._

class AllocatorReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid = Bool()
  val is_ld = Bool()
  val is_st = Bool()
}

class AllocatorResp(implicit p: Parameters) extends BaseZirconBundle {
  val rob_id = UInt(robIdBits.W)
  val ld_id  = UInt(ldqIdBits.W)
  val st_id  = UInt(stqIdBits.W)
  val rsv_id = UInt(rsvIdBits.W)
}

class AllocatorDealloc(implicit p: Parameters) extends AllocatorReq

class AllocatorKill(implicit p: Parameters) extends BaseZirconBundle {
  val rob_id = UInt(robIdBits.W)
  val ld_id  = UInt(ldqIdBits.W)
  val st_id  = UInt(stqIdBits.W)
}


class Allocator(plWidth: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    //  Flush
    val flush = Input(Bool())

    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, new AllocatorReq)))
    val resps = Output(Vec(plWidth, new AllocatorResp))

    //  Dealloc
    val dealloc = Flipped(Valid(Vec(plWidth, new AllocatorDealloc)))
    val rsv_dealloc = Flipped(Valid(Vec(numRsvEntries, Bool())))

    //  Empty signal
    val empty = Output(Bool())
  })

  //
  val rob_freelist = Module(new FreeList(plWidth, numRobEntries))
  val ldq_freelist = Module(new FreeList(plWidth, numLdqEntries))
  val stq_freelist = Module(new FreeList(plWidth, numStqEntries))
  val rsv_freelist = Module(new IssueFreeList(plWidth, numRsvEntries))

  //  Re-Order Buffer Free List
  rob_freelist.io.flush := io.flush
  rob_freelist.io.reqs.valid := io.reqs.valid
  rob_freelist.io.reqs.bits := io.reqs.bits.map(_.valid)
  rob_freelist.io.dealloc_reqs.valid := io.dealloc.valid
  rob_freelist.io.dealloc_reqs.bits := io.dealloc.bits.map(_.valid)

  val rob_entries = rob_freelist.io.alloc_entry

  //  Load-Queue Free List
  ldq_freelist.io.flush := io.flush
  ldq_freelist.io.reqs.valid := io.reqs.valid
  ldq_freelist.io.reqs.bits := io.reqs.bits.map(_.is_ld)
  ldq_freelist.io.dealloc_reqs.valid := io.dealloc.valid
  ldq_freelist.io.dealloc_reqs.bits := io.dealloc.bits.map(_.is_ld)

  val ldq_entries = ldq_freelist.io.alloc_entry

  //  Store-Queue Free List
  stq_freelist.io.flush := io.flush
  stq_freelist.io.reqs.valid := io.reqs.valid
  stq_freelist.io.reqs.bits := io.reqs.bits.map(_.is_st)
  stq_freelist.io.dealloc_reqs.valid := io.dealloc.valid
  stq_freelist.io.dealloc_reqs.bits := io.dealloc.bits.map(_.is_st)

  val stq_entries = stq_freelist.io.alloc_entry

  //  Reservation Station Free List
  rsv_freelist.io.kill := io.flush
  rsv_freelist.io.reqs.valid := io.reqs.valid
  rsv_freelist.io.reqs.bits := io.reqs.bits.map(_.valid)
  rsv_freelist.io.dealloc_reqs := io.rsv_dealloc

  val rsv_entries = rsv_freelist.io.resps

  for (w <- 0 until plWidth) {
    io.resps(w).rob_id := rob_entries(w)
    io.resps(w).ld_id := ldq_entries(w)
    io.resps(w).st_id := stq_entries(w)
    io.resps(w).rsv_id := rsv_freelist.io.resps(w)
  }
  io.empty := (rob_freelist.io.empty || ldq_freelist.io.empty || stq_freelist.io.empty || rsv_freelist.io.empty)
}