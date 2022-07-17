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
package zircon.axi4

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import zircon.common.HasZirconCoreParameters

import scala.collection.mutable._

object ClintConstants {
  def timeOffset = 0xbff8
  def msipBytes = 4
  def timecmpBytes = 8
  def size = 0x10000
  def timeWidth = 64
  def ipiWidth = 32
  def ints = 2

  val msip = 0x02000000
  val timecmp_lo = 0x02004000
  val timecmp_hi = 0x02004004
  val time_lo = 0x0200bff8
  val time_hi = 0x0200bffc

  def msipOffset(hart: Int) = hart * msipBytes
  def timecmpOffset(hart: Int) = 0x4000 + hart * timecmpBytes
}

class ClintIO extends Bundle {
  val rtc_tick = Input(Bool())
  val timer_int = Output(Bool())
  val sft_int = Output(Bool())
}

class AXI4Clint(params: AXI4SlaveParams, extendIO: ClintIO)(implicit p: Parameters) extends BaseAXI4SlaveModule(params, extendIO) with HasZirconCoreParameters{
  override lazy val module = new AXI4SlaveModuleImp(this) {
    import ClintConstants._
    val ext_io = io.extend_io.get
    assert(ext_io != None)

    val time_reg = RegInit(UInt(timeWidth.W), 0.U(timeWidth.W))
    when (ext_io.rtc_tick) { time_reg := time_reg + 1.U }
    val timecmp_lo_reg = RegInit(UInt(32.W), 0.U(32.W))
    val timecmp_hi_reg = RegInit(UInt(32.W), 0.U(32.W))
    val msip_reg = RegInit(UInt(ipiWidth.W), 0.U(ipiWidth.W))

    //  Interrupts
    ext_io.sft_int := msip_reg(0)
    ext_io.timer_int := time_reg >= Cat(timecmp_hi_reg, timecmp_lo_reg)

    //
    val read_mapping = LinkedHashMap[Int, Bits] (
      0x02000000 -> msip_reg,
      0x02004000 -> timecmp_lo_reg,
      0x02004004 -> timecmp_hi_reg,
      0x0200bff8 -> time_reg(31, 0),
      0x0200bffc -> time_reg(timeWidth - 1, 32)
    )

    val rch_decoded_addr = read_mapping map { case (k, v) => k -> (k.U === in.aw.bits.addr(vaddrBits-1,0)) }
    r_bits.id := ar_bits_reg.id
    r_bits.data := Mux1H(for ((k, v) <- read_mapping) yield rch_decoded_addr(k) -> v)
    r_bits.resp := Mux((for ((k, v) <- read_mapping) yield rch_decoded_addr(k)).reduce(_|_), in.params.RESP_DECERR, in.params.RESP_OKAY)
    r_bits.last := read_cnt === 0.U | r_bits.resp.orR

    //  Write
    val wch_decoded_addr = read_mapping map { case (k, v) => k -> (in.aw.bits.addr(vaddrBits-1,0) === k.U)}

    b_bits.id := aw_bits_reg.id
    b_bits.resp := Mux((for ((k, v) <- read_mapping) yield wch_decoded_addr(k)).reduce(_|_), in.params.RESP_DECERR, in.params.RESP_OKAY)

    when (mem_wr_en) {
      when (wch_decoded_addr(ClintConstants.msip)) {
        msip_reg := Cat(Fill(31, 0.U), in.w.bits.data(0))
      }
      when (wch_decoded_addr(ClintConstants.timecmp_lo)) {
        timecmp_lo_reg := in.w.bits.data
      }
      when (wch_decoded_addr(ClintConstants.timecmp_hi)) {
        timecmp_hi_reg := in.w.bits.data
      }
    }
  }
}