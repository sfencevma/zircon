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
package zircon.decode.bpd

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import zircon.common._
import zircon.util._


class RAS(numRasEntries: Int)(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new Bundle() {
    val read_vld = Input(Bool())
    val read_addr = Output(UInt(vaddrBits.W))
    val write_vld = Input(Bool())
    val write_addr = Input(UInt(vaddrBits.W))
  })

  def idxBits: Int = log2Ceil(numRasEntries) + 1
  /**
   *    RAS is occupied by the return address of the same CALL instruction, and the
   * same duplicate value is saved. This is actually a waste of RAS space. For the
   * same CALL instruction that is executed consecutively (the continuity here refers
   * to that the CALL instructions executed two times adjacent to each other are the
   * same instruction), their return addresses can be stored in the same place in RAS,
   * and a counter is used to mark the number of times the CALL instruction is executed.
   * For example, using an 8-bit counter can mark up to 256 levels of recursive CALLs,
   * which is equivalent to expanding the capacity of RAS, The accuracy of prediction
   * is increased.
   *    When this RAS with counter works, it will compare the return address of the
   * CALL instruction written into the RAS with the last written return address. If
   * they are equal, it means that the same CALL instruction is executed both times.
   * At this time, the current poison pointer of Ras will point to the counter of
   * the table entry plus 1, and this pointer will remain unchanged; When encountering
   * the return instruction, read the data from RAS in the previous way, which is the
   * target address of the RETURN instruction. At the same time, reduce the current
   * counter of RAS by 1. If the counter is 0, it means that this table entry has ended
   * the recursive CALL. At this time, you can make the read pointer of Ras point to the next CALL.
   */

  class RASEntry extends Bundle {
    val cnt   = UInt(8.W)
    val addr  = UInt(vaddrBits.W)
  }

  val ras = Reg(Vec(numRasEntries, new RASEntry))
  val ras_ptr = Reg(UInt(idxBits.W))
  val read_idx = hashIdx(ras_ptr - 1.U)
  val write_idx = hashIdx(ras_ptr)

  //
  val last_addr = RegEnable(io.write_addr, io.write_vld)

  //  Update RAS Top
  when (io.write_vld && last_addr =/= io.write_addr) {
    ras_ptr := ras_ptr + 1.U
  } .elsewhen (io.read_vld && ras(read_idx).cnt === 0.U) {
    ras_ptr := ras_ptr - 1.U
  } .otherwise {
    ras_ptr := ras_ptr
  }

  //  Update Counter
  when (io.read_vld && ras(read_idx).cnt =/= 0.U) {
    ras(read_idx).cnt := ras(read_idx).cnt - 1.U
  }

  when (io.write_vld) {
    when (last_addr =/= io.write_addr) {
      val new_entry = Wire(new RASEntry)
      new_entry.cnt := 0.U
      new_entry.addr := io.write_addr
      ras(write_idx) := new_entry
    } .otherwise {
      ras(read_idx).cnt := ras(read_idx).cnt + 1.U
    }
  }

  //  Read  Return Address
  io.read_addr := ras(read_idx).addr
}