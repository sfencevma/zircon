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

abstract class BaseAXI4SlaveModule[T <: Data](params: AXI4SlaveParams, val extendIO: T = null)(implicit p: Parameters) extends LazyModule {
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParams(Seq(params))))
  val module = new AXI4SlaveModuleImp[T](this)
}

class AXI4SlaveModuleImp[T <: Data](outer: BaseAXI4SlaveModule[T]) extends LazyModuleImp(outer) {
  val io = IO(new Bundle() {
    val extend_io = if (outer.extendIO == null) None else Some(outer.extendIO.cloneType)
  })

  val (in, _) = outer.node.in.head

  //  AXI4 Slave Implement Here
  //  FSM
  val s_idle::s_burst::s_resp::Nil = Enum(3)

  //  AW Channel
  val aw_fire = in.aw.fire
  val aw_bits_reg = RegEnable(in.aw.bits, aw_fire)
  val aw_ready_reg = RegInit(Bool(), false.B)

  //  W Channel
  val w_fire = in.w.fire
  val w_ready_reg = RegInit(Bool(), false.B)

  //  B Channel
  val b_fire = in.b.fire
  val b_bits = Wire(new AXI4BundleB(in.params))
  val b_bits_reg = Reg(new AXI4BundleB(in.params))
  val b_valid_reg = RegInit(Bool(), false.B)

  //
  val w_state_reg = RegInit(s_idle)
  val write_addr = Reg(UInt(in.params.addrBits.W))
  val write_cnt = Reg(UInt(in.params.lenBits.W))

  //  Set Default
  val mem_wr_en = w_fire
  aw_ready_reg := false.B
  b_valid_reg := b_valid_reg & !in.b.ready

  switch (w_state_reg) {
    is (s_idle) {
      aw_ready_reg := true.B
      when (aw_fire) {
        aw_ready_reg  := false.B
        w_ready_reg   := true.B
        write_addr    := in.aw.bits.addr
        write_cnt     := in.aw.bits.len
        w_state_reg   := s_burst
      }
    }

    is (s_burst) {
      when (w_fire) {
        when (aw_bits_reg.burst =/= in.params.BURST_FIXED) {
          write_addr := write_addr + (1.U << aw_bits_reg.size)
        }

        write_cnt := write_cnt - 1.U
        when (write_cnt === 0.U) {
          w_ready_reg := false.B
          when (in.b.ready || !b_valid_reg) {
            b_valid_reg   := true.B
            aw_ready_reg  := true.B
            b_bits_reg    := b_bits
            w_state_reg   := s_idle
          }
        }
      } .otherwise {
        w_ready_reg := true.B
        w_state_reg := s_burst
      }
    }

    is (s_resp) {
      when (in.b.ready || !b_valid_reg) {
        b_valid_reg   := true.B
        aw_ready_reg  := true.B
        w_state_reg   := s_idle
      }
    }
  }

  in.aw.ready := aw_ready_reg
  in.w.ready  := w_ready_reg
  in.b.valid  := b_valid_reg
  in.b.bits   := b_bits_reg


  //  Read
  //  AR Channel
  val ar_fire = in.ar.fire
  val ar_bits_reg = RegEnable(in.ar.bits, ar_fire)
  val ar_ready_reg = RegInit(Bool(), false.B)

  //  R Channel
  val r_valid_reg = RegInit(Bool(), false.B)
  val r_bits = Wire(new AXI4BundleR(in.params))
  val r_bits_reg = Reg(new AXI4BundleR(in.params))
  val r_state_reg = RegInit(s_idle)
  val read_addr = Reg(UInt(in.params.addrBits.W))
  val read_cnt = Reg(UInt(in.params.lenBits.W))

  //  Set Default
  val mem_rd_en = in.r.ready || !r_valid_reg
  ar_ready_reg := false.B
  r_valid_reg := r_valid_reg && !in.r.ready

  switch (r_state_reg) {
    is (s_idle) {
      ar_ready_reg := true.B
      when (ar_fire) {
        read_addr     := in.ar.bits.addr
        read_cnt      := in.ar.bits.len
        ar_ready_reg  := false.B
        r_state_reg   := s_burst
      }
    }

    is (s_burst) {
      when (in.r.ready || !r_valid_reg) {
        r_valid_reg := true.B
        r_bits_reg  := r_bits
        when (ar_bits_reg.burst =/= in.params.BURST_FIXED) {
          read_addr := read_addr + (1.U << ar_bits_reg.size)
        }
        read_cnt := read_cnt + 1.U
        when (read_cnt === 0.U) {
          ar_ready_reg := true.B
          r_state_reg := s_idle
        }
      }
    }
  }

  in.ar.ready := ar_ready_reg
  in.r.valid  := r_valid_reg
  in.r.bits   := r_bits_reg
}


abstract class BaseAXI4MasterModule[T <: Data](params: Seq[AXI4MasterParams], val extendIO: T = null)(implicit p: Parameters) extends LazyModule {
  val node = AXI4MasterNode(Seq(AXI4MasterPortParams(params)))
  val module = new AXI4MasterModuleImp[T](this)
}

class AXI4MasterModuleImp[T <: Data](outer: BaseAXI4MasterModule[T]) extends LazyModuleImp(outer) {
  val io = IO(new Bundle() {
    val extend_io = if (outer.extendIO == null) None else Some(outer.extendIO.cloneType)
  })


  //  val (out, _) = outer.node.out.head
  //  AXI4 Master Implement Here
}
