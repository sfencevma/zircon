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

class IssueAgeMatrixReq(implicit p: Parameters) extends BaseZirconBundle {
  val valid   = Bool()
  val rsv_id  = UInt(rsvIdBits.W)
}

class IssueAgeMatrix(plWidth: Int, numEntries: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val reqs = Flipped(Valid(Vec(plWidth, new IssueAgeMatrixReq)))
    val valid_vec = Input(Vec(numEntries, Bool()))
    val oldest_vec = Output(Vec(numEntries, Bool()))
  })

  //
  val matrix = Reg(Vec(numEntries, UInt(numEntries.W)))

  when (io.flush) {
    matrix.foreach(_ := 0.U)
  } .elsewhen (io.reqs.valid) {
    for (w <- 0 until plWidth) {
      var mask = Reverse(Cat(io.valid_vec))
      for (i <- 0 until plWidth) {
        if (i < w) {
          mask = mask | UIntToOH(io.reqs.bits(w).rsv_id)
        } else  {
          mask = mask
        }
      }
      when (io.reqs.bits(w).valid) {
        matrix(io.reqs.bits(w).rsv_id) := mask
      }
    }
  }

  for (e <- 0 until numEntries) {
    var bit_mask = false.B
    for (i <- 0 until numEntries) {
      if (i == e) {
        bit_mask = bit_mask
      } else {
        bit_mask = bit_mask | (io.valid_vec(i) & matrix(e)(i))
      }
    }
    io.oldest_vec(e) := io.valid_vec(e) & ~bit_mask
  }
}