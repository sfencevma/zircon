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
import zircon.common._
import scala.collection.mutable.LinkedHashMap


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

class AXI4Clint(implicit p: Parameters) extends BaseZirconModule {
  import ClintConstants._

  val io = IO(new Bundle() {
    val node = AXI4SlaveBundle(mmapBusParams)

    val rtc_tick = Input(Bool())
    val timer_int = Output(Bool())
    val sft_int = Output(Bool())
  })

  val io_in = io.node
  val device = new SimpleDevice("clint", Seq("riscv.clint")) {
    override val alwaysExtended: Boolean = true
  }

  val time_reg = RegInit(UInt(timeWidth.W), 0.U(timeWidth.W))
  when(io.rtc_tick) {
    time_reg := time_reg + 1.U
  }
  val timecmp_lo_reg = RegInit(UInt(32.W), 0.U(32.W))
  val timecmp_hi_reg = RegInit(UInt(32.W), 0.U(32.W))
  val msip_reg = RegInit(UInt(ipiWidth.W), 0.U(ipiWidth.W))

  //  Interrupts
  io.sft_int := msip_reg(0)
  io.timer_int := time_reg >= Cat(timecmp_hi_reg, timecmp_lo_reg)

  //
  val read_mapping = LinkedHashMap[Int, UInt](
    0x02000000 -> msip_reg,
    0x02004000 -> timecmp_lo_reg,
    0x02004004 -> timecmp_hi_reg,
    0x0200bff8 -> time_reg(31, 0),
    0x0200bffc -> time_reg(timeWidth - 1, 32)
  )


  //  AXI4 Slave
  val s_ready::s_burst::s_resp::Nil = Enum(3)

  //  AW Channel
  val aw_fire = io_in.aw.fire
  val aw_bits_reg = RegEnable(io_in.aw.bits, aw_fire)
  val aw_ready_reg = RegInit(Bool(), false.B)

  //  W Channel
  val w_fire = io_in.w.fire
  val w_ready_reg = RegInit(Bool(), false.B)

  //  B Channel
  val b_fire = io_in.b.fire
  val b_bits = Wire(new AXI4BundleB(io_in.params))
  val b_bits_reg = Reg(new AXI4BundleB(io_in.params))
  val b_valid_reg = RegInit(Bool(), false.B)

  val w_state_reg = RegInit(s_ready)
  val write_addr = Reg(UInt(io_in.params.addrBits.W))
  val write_cnt = Reg(UInt(io_in.params.lenBits.W))

  //  Set Default
  val mem_wr_en = w_fire
  aw_ready_reg := false.B
  b_valid_reg := b_valid_reg & !io_in.b.ready

  switch (w_state_reg) {

    is (s_ready) {
      aw_ready_reg := true.B
      when (aw_fire) {
        aw_ready_reg  := false.B
        w_ready_reg   := true.B
        write_addr    := io_in.aw.bits.addr
        write_cnt     := io_in.aw.bits.len
        w_state_reg   := s_burst
      }
    }

    is (s_burst) {
      when (w_fire) {
        when (aw_bits_reg.burst =/= io_in.params.BURST_FIXED) {
          write_addr := write_addr + (1.U << aw_bits_reg.size)
        }

        write_cnt := write_cnt - 1.U
        when (write_cnt === 0.U) {
          w_ready_reg := false.B
          when (io_in.b.ready || !b_valid_reg) {
            b_valid_reg   := true.B
            aw_ready_reg  := true.B
            b_bits_reg    := b_bits
            w_state_reg   := s_ready
          }
        }
      } .otherwise {
        w_ready_reg := true.B
        w_state_reg := s_burst
      }
    }

    is (s_resp) {
      when (io_in.b.ready || !b_valid_reg) {
        b_valid_reg   := true.B
        aw_ready_reg  := true.B
        w_state_reg   := s_ready
      }
    }

  }

  io_in.aw.ready  := aw_ready_reg
  io_in.w.ready   := w_ready_reg
  io_in.b.valid   := b_valid_reg
  io_in.b.bits    := b_bits_reg

  //
  //  AR Channel
  val ar_fire = io_in.ar.fire
  val ar_bits_reg = RegEnable(io_in.ar.bits, ar_fire)
  val ar_ready_reg = RegInit(Bool(), false.B)

  //  R Channel
  val r_valid_reg = RegInit(Bool(), false.B)
  val r_bits = Wire(new AXI4BundleR(io_in.params))
  val r_bits_reg = Reg(new AXI4BundleR(io_in.params))
  val r_state_reg = RegInit(s_ready)
  val read_addr = Reg(UInt(io_in.params.addrBits.W))
  val read_cnt = Reg(UInt(io_in.params.lenBits.W))

  //  Set Default
  val mem_rd_en = io_in.r.ready || !r_valid_reg
  ar_ready_reg := false.B
  r_valid_reg := r_valid_reg && !io_in.r.ready

  switch (r_state_reg) {

    is (s_ready) {
      ar_ready_reg := true.B
      when (ar_fire) {
        read_addr     := io_in.ar.bits.addr
        read_cnt      := io_in.ar.bits.len
        ar_ready_reg  := false.B
        r_state_reg   := s_burst
      }
    }

    is (s_burst) {
      when (io_in.r.ready || !r_valid_reg) {
        r_valid_reg := true.B
        r_bits_reg  := r_bits
        when (ar_bits_reg.burst =/= io_in.params.BURST_FIXED) {
          read_addr := read_addr + (1.U << ar_bits_reg.size)
        }
        read_cnt := read_cnt + 1.U
        when (read_cnt === 0.U) {
          ar_ready_reg := true.B
          r_state_reg := s_ready
        }
      }
    }

  }

  io_in.ar.ready := ar_ready_reg
  io_in.r.valid  := r_valid_reg
  io_in.r.bits   := r_bits_reg

  //  Read
  val rch_decoded_addr = read_mapping map { case (k, v) => k -> (k.U === io_in.aw.bits.addr(io_in.params.addrBits - 1, 0)) }
  r_bits.id := ar_bits_reg.id
  r_bits.data := Mux1H(for ((k, v) <- read_mapping) yield rch_decoded_addr(k) -> v)
  r_bits.resp := Mux((for ((k, v) <- read_mapping) yield rch_decoded_addr(k)).reduce(_ | _), io_in.params.RESP_DECERR, io_in.params.RESP_OKAY)
  r_bits.user := ar_bits_reg.user
  r_bits.last := read_cnt === 0.U | r_bits.resp.orR //  FIXME: return when has a cause ?
  io_in.r.bits := r_bits

  //  Write
  val wch_decoded_addr = read_mapping map { case (k, v) => k -> (io_in.aw.bits.addr(io_in.params.addrBits - 1, 0) === k.U) }

  b_bits.id   := aw_bits_reg.id
  b_bits.user := aw_bits_reg.user
  b_bits.resp := Mux((for ((k, v) <- read_mapping) yield wch_decoded_addr(k)).reduce(_ | _), io_in.params.RESP_DECERR, io_in.params.RESP_OKAY)
  io_in.b.bits := b_bits

  when(mem_wr_en) {
    when(wch_decoded_addr(ClintConstants.msip)) {
      msip_reg := Cat(Fill(31, 0.U), io_in.w.bits.data(0))
    }
    when(wch_decoded_addr(ClintConstants.timecmp_lo)) {
      timecmp_lo_reg := io_in.w.bits.data(31, 0)
    }
    when(wch_decoded_addr(ClintConstants.timecmp_hi)) {
      timecmp_hi_reg := io_in.w.bits.data(31, 0)
    }
  }

  //  End
}