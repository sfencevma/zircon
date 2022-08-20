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
import chisel3.util.experimental.loadMemoryFromFile
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import firrtl.annotations._
import zircon.common._

class RAMHelper(memByte: Int, idxBits: Int, dataBits: Int) extends BlackBox {
  val io = IO(new Bundle() {
    val clk = Input(Clock())
    val rIdx = Input(UInt(idxBits.W))
    val rdata = Output(UInt(dataBits.W))
    val wIdx = Input(UInt(idxBits.W))
    val wdata = Input(UInt(dataBits.W))
    val wmask = Input(UInt((dataBits/8).W))
    val wen = Input(Bool())
    val en = Input(Bool())
  }).suggestName("io")
}

class AXI4Ram(memByte: Int, useBlackBox: Boolean = false)(implicit p: Parameters) extends BaseZirconModule {

  val io = IO(new Bundle() {
    val node = AXI4SlaveBundle(memBusParams)
  })

  val device = new SimpleDevice("memory", Seq("riscv.memory")) {
    override val alwaysExtended: Boolean = true
  }
  val io_in = io.node

  //  AXI4 Slave
  val s_ready :: s_burst :: s_resp :: Nil = Enum(3)

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

  switch(w_state_reg) {
    is(s_ready) {
      aw_ready_reg := true.B
      when(aw_fire) {
        aw_ready_reg := false.B
        w_ready_reg := true.B
        write_addr := io_in.aw.bits.addr
        write_cnt := Mux(io_in.aw.bits.len === 0.U, 0.U, io_in.aw.bits.len - 1.U)
        w_state_reg := s_burst
      }
    }

    is(s_burst) {
      when(w_fire) {
        when(aw_bits_reg.burst =/= io_in.params.BURST_FIXED) {
          write_addr := write_addr + (1.U << aw_bits_reg.size)
        }

        when(write_cnt === 0.U) {
          w_ready_reg := false.B
          when(io_in.b.ready || !b_valid_reg) {
            b_valid_reg := true.B
            aw_ready_reg := true.B
            b_bits_reg := b_bits
            w_state_reg := s_ready
          }
        }
        write_cnt := write_cnt - 1.U
      }.otherwise {
        w_ready_reg := true.B
        w_state_reg := s_burst
      }
    }

    is(s_resp) {
      when(io_in.b.ready || !b_valid_reg) {
        b_bits_reg := b_bits
        b_valid_reg := true.B
        aw_ready_reg := true.B
        w_state_reg := s_ready
      }
    }
  }

  io_in.aw.ready := aw_ready_reg
  io_in.w.ready := w_ready_reg
  b_bits.id     := aw_bits_reg.id
  b_bits.resp   := io_in.params.RESP_OKAY
  b_bits.user   := aw_bits_reg.user
  io_in.b.valid := b_valid_reg
  io_in.b.bits := b_bits_reg

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
  val mem_rd_en = ar_ready_reg || !r_valid_reg

  ar_ready_reg := false.B
  r_valid_reg := r_valid_reg && !io_in.r.ready

  switch(r_state_reg) {
    is(s_ready) {
      ar_ready_reg := true.B
      when(ar_fire) {
        read_addr := io_in.ar.bits.addr
        read_cnt := Mux(io_in.ar.bits.len === 0.U, 0.U, io_in.ar.bits.len - 1.U)
        ar_ready_reg := false.B
        r_state_reg := s_burst
      }
    }

    is(s_burst) {
      when(io_in.r.ready || !r_valid_reg) {
        r_valid_reg := true.B
        r_bits_reg := r_bits
        when(ar_bits_reg.burst =/= io_in.params.BURST_FIXED) {
          read_addr := read_addr + (1.U << ar_bits_reg.size)
        }
        when(read_cnt === 0.U) {
          ar_ready_reg := true.B
          r_state_reg := s_ready
        }
        read_cnt := read_cnt - 1.U
      }
    }
  }

  //  Ram implement
  def DataByte: Int = io_in.params.dataBits / 8
  def fullMask: Int = (1 << DataByte) - 1
  def offBits : Int = log2Up(memByte)
  def offMask : Int = (1 << offBits) - 1
  def index(addr: UInt) = ((addr & offMask.U) >> log2Ceil(DataByte)).asUInt


  val wIdx = index(aw_bits_reg.addr) + write_cnt
  val rIdx = index(read_addr) + read_cnt

  val read_data = if (useBlackBox) {
    val mem = Module(new RAMHelper(memByte, rIdx.getWidth, DataByte * 8))
    mem.io.clk    := clock
    mem.io.rIdx   := rIdx
    mem.io.wIdx   := wIdx
    mem.io.wdata  := io_in.w.bits.data
    mem.io.wmask  := fullMask.U
    mem.io.wen    := io_in.w.fire && (wIdx < (memByte / 8 ).U)
    mem.io.en     := true.B
    mem.io.rdata
  } else {
    val ram = SyncReadMem(memByte / DataByte, Vec(DataByte, UInt(8.W)))
    // loadMemoryFromFile(ram, "./dummy-riscv64-nemu.bin", MemoryLoadFileType.Binary)

    //  Write
    val wdata = Seq.tabulate(DataByte) { i => io_in.w.bits.data(8 * (i + 1) - 1, 8 * i) }
    when(mem_wr_en) {
      ram.write(wIdx, VecInit(wdata), io_in.w.bits.strb.asBools)
    }

    //  Read
    Cat(ram.read(rIdx, mem_rd_en).reverse)
  }

  io_in.ar.ready  := ar_ready_reg
  io_in.r.valid   := r_valid_reg
  r_bits.id       := ar_bits_reg.id
  r_bits.user     := ar_bits_reg.user
  r_bits.data     := read_data
  r_bits.resp     := io_in.params.RESP_OKAY   //  Always OK.
  r_bits.last     := read_cnt === 0.U
  io_in.r.bits    := r_bits_reg

}