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

class FreeList(
                plWidth: Int,
                numEntries: Int
              )(implicit p: Parameters) extends BaseZirconModule {
  val idxBits: Int = log2Ceil(numEntries) + 1

  val io = IO(new Bundle() {
    //  Flush
    val flush = Input(Bool())

    //  Require
    val reqs = Flipped(Valid(Vec(plWidth, Bool())))
    val alloc_entry = Output(Vec(plWidth, UInt(idxBits.W)))
    val dealloc_reqs = Flipped(Valid(Vec(plWidth, Bool())))

    //  Empty signal
    val empty = Output(Bool())
  })


  val head = RegInit(UInt(idxBits.W), 0.U(idxBits.W))
  val tail = RegInit(UInt(idxBits.W), 0.U(idxBits.W))

  //  Require
  var req_idx = tail
  for (w <- 0 until plWidth) {
    io.alloc_entry(w) := req_idx
    req_idx = req_idx + io.reqs.bits(w)
  }

  val req_count = PopCount(Cat(io.reqs.bits))
  io.empty := req_count > (numEntries.U - (tail - head))

  //  Deallocation
  val dealloc_count = PopCount(Cat(io.dealloc_reqs.bits))

  when (io.flush) {
    head := 0.U
  } .elsewhen (io.dealloc_reqs.valid) {
    head := head + dealloc_count
  }

  when (io.flush) {
    tail := 0.U
  } .elsewhen (!io.empty && io.reqs.valid) {
    tail := tail + req_count
  }

}