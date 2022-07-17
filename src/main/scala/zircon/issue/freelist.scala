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
import zircon.util._

class IssueFreeList(
                     val plWidth: Int,
                     val numEntries: Int
                   )(implicit p: Parameters) extends BaseZirconModule {
  private val idxBits = log2Ceil(numEntries)

  val io = IO(new Bundle() {
    //  Flush
    val kill = Input(Bool())

    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, Bool())))
    val resps = Output(Vec(plWidth, UInt(idxBits.W)))

    //  Dealloc
    val dealloc_reqs = Flipped(Valid(Vec(numEntries, Bool())))

    //  Empty signal
    val empty = Output(Bool())
  })

  val free_list = RegInit(UInt(numEntries.W), 0.U(numEntries.W))


  //  Allocation
  val sels = selectFirst(free_list, plWidth)
  val sel_mask = (sels zip io.reqs.bits) map { case (s, r) => s & Fill(numEntries, io.reqs.valid & r) } reduce(_|_)
  when (io.kill) {
    free_list := 0.U
  } .otherwise {
    free_list := (free_list & ~sel_mask | (Fill(numEntries, io.dealloc_reqs.valid) & Reverse(Cat(io.dealloc_reqs.bits))))
  }

  io.resps := sels.map(OHToUInt(_))
  io.empty := (PopCount(io.reqs.bits.map(_ && io.reqs.valid)) > PopCount(free_list))

}