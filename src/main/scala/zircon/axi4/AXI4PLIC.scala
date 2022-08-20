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
import freechips.rocketchip.util.{MuxT, groupByIntoSeq}
import zircon.common._
import zircon.utils._

object PLICConstants {
  val nWdogs = 1
  val nRtcs = 1
  val nUarts = 2
  val nQspis = 3
  val nGpios = 32
  val nPwms = 12

  val maxDevices = 1023
  val maxHarts = 15872
  val nDevices = nWdogs + nRtcs + nUarts + nQspis + nGpios + nPwms
  val nHarts = 1

  val plicBase = 0x0c000000
  val priorityBase = 0x0
  val pendingBase = 0x1000
  val enableBase = 0x2000
  val thresholdBase = 0x200000
  val claimCompleteBase = 0x200004


  def enableOffset(i: Int) = plicBase + i * 0x80
  def enableBase(i: Int, w: Int): Int = plicBase + enableBase + enableOffset(i) + 4 * w
  def thresholdOffset(i: Int): Int = i * 0x1000
  def thresholdBase(i: Int): Int = plicBase + thresholdBase + thresholdOffset(i)
  def claimCompleteOffset(i: Int): Int = i * 0x1000
  def claimCompleteBase(i: Int): Int = plicBase + claimCompleteBase + claimCompleteOffset(i)
}

class GatewaysIO extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val complete = Input(Bool())
}

class LevelGateway extends Module {
  val io = IO(new Bundle() {
    val interrupt = Input(Bool())
    val plic = new GatewaysIO
  })

  val int_req = RegInit(Bool(), false.B)
  when (io.interrupt & io.plic.ready) { int_req := true.B }
  when (io.plic.complete) { int_req := false.B }
  io.plic.valid := io.interrupt && !int_req
}

class AXI4Plic(implicit p: Parameters) extends BaseZirconModule {
  import PLICConstants._

  val io = IO(new Bundle() {
    val node = AXI4SlaveBundle(mmapBusParams)

    val wdogcmp = Vec(PLICConstants.nWdogs, Flipped(new GatewaysIO))
    val rtccmp  = Vec(PLICConstants.nRtcs, Flipped(new GatewaysIO))
    val uart    = Vec(PLICConstants.nUarts, Flipped(new GatewaysIO))
    val qspi    = Vec(PLICConstants.nQspis, Flipped(new GatewaysIO))
    val gpio    = Vec(PLICConstants.nGpios, Flipped(new GatewaysIO))
    val pwm     = Vec(PLICConstants.nPwms, Flipped(new GatewaysIO))

    //
    val harts  = Output(Vec(nHarts, Bool()))
  })


  val device = new SimpleDevice("plic", Seq("riscv.plic")) {
    override val alwaysExtended: Boolean = true
  }

  val io_in = io.node
  def nDevices = PLICConstants.nDevices
  def minPriorities = nDevices max 7
  def nPriorities = (1 << log2Ceil(minPriorities + 1)) - 1
  def addressSpaceSize = 0x4000000
  def addressBits = log2Up(addressSpaceSize)
  def getOffset(addr: UInt) = addr(addressBits - 1, 0)
  val interrupts = io.wdogcmp ++ io.rtccmp ++ io.uart ++ io.qspi ++ io.gpio ++ io.pwm
  val gateways = interrupts.map { case i =>
    val gateway = Module(new LevelGateway)
    gateway.io.interrupt := i.valid
    i.ready := gateway.io.plic.ready
    i.complete := gateway.io.plic.complete
    gateway.io.plic
  }

  def priorityRegField(x: UInt) = {
    if (nPriorities > 0) {
      RegMapField(32, x)
    } else {
      RegMapField.r(32, x)
    }
  }

  def thresholdRegField(x: UInt) = {
    if (nPriorities > 0) {
      RegMapField(32, x)
    } else {
      RegMapField.r(32, x)
    }
  }
  def findMax(x: Seq[UInt]): (UInt, UInt) = {
    if (x.length > 1) {
      val half = 1 << (log2Ceil(x.length) - 1)
      val left = findMax(x take half)
      val right = findMax(x drop half)
      MuxT(left._1 >= right._1, left, (right._1, half.U | right._2))
    } else {
      (x.head, 0.U)
    }
  }
  def rawFanIn(nDevices: Int, prioBits: Int)(prio: Vec[UInt], ip: UInt): (UInt, UInt) = {
    val effectPriority = (1.U << prioBits) +: (ip.asBools zip prio).map {case (p, x) => Cat(p, x) }
    findMax(effectPriority)
  }
  def fanIn(prio: Vec[UInt], ip: UInt): (UInt, UInt) = rawFanIn(nDevices, 32)(prio, ip)

  val priority = Reg(Vec(nDevices, UInt(32.W)))
  val priorityRegFields = priority.zipWithIndex.map { case (r, i) =>
    PLICConstants.priorityBase + 4 * (i + 1) -> priorityRegField(r)
  }
  val devs = (nDevices + 31) / 32
  val pendingRegs = Reg(Vec(32 * devs, Bool()))
  val pendingUInt = Reverse(Cat(pendingRegs))
  val pending = (0 until devs).map(i => pendingUInt(32 * (i + 1) - 1, 32 * i)) // Reg(Vec(devs, UInt(32.W)))
  val pendingRegFields = pending.zipWithIndex.map { case (r, i) =>
    PLICConstants.pendingBase + 4 * i -> RegMapField.r(32, r)
  }
  val enableRegs = Seq.fill(devs)(Reg(UInt(32.W)))
  val enables = Seq.fill(nHarts) { enableRegs }
  val enable_vec = VecInit(enables.map(s => Cat(s.reverse)))
  val enable_vec_0 = VecInit(enable_vec.map(x => Cat(x, 0.U(1.W))))
  val enableRegFields = enables.zipWithIndex.map { case (f, hart) =>
    f.zipWithIndex.map { case (r, w) => PLICConstants.enableBase(hart, w) -> RegMapField(32, r) }
  }.reduce(_ ++ _).toMap
  val threshold = Reg(Vec(nHarts, UInt(32.W)))
  val thresholdRegFields = Seq.tabulate(nHarts) { i =>
    PLICConstants.thresholdBase(i) -> thresholdRegField(threshold(i))
  }
  val harts = Reg(Vec(nHarts, Bool()))
  val maxDevs = Wire(Vec(nHarts, UInt(log2Up(nDevices).W)))
  val pending_vec = Cat(pending.reverse)

  for (hart <- 0 until nHarts) {
    val (maxDev, th) = fanIn(priority, enable_vec(hart) & pending_vec)
    maxDevs(hart) := maxDev
    harts(hart) := th > threshold(hart)
  }

  io.harts := harts


  val claimer = Wire(Vec(nHarts, Bool()))
  val claimming = Seq.tabulate(nHarts) { i => Mux(claimer(i), maxDevs(i), 0.U) }.reduce(_ | _)
  val claimedDevs = VecInit(UIntToOH(claimming).asBools)
  val completer = Wire(Vec(nHarts, Bool()))
  val completerDev = Wire(UInt(log2Up(nDevices).W))
  val completedDevs = Mux(completer.reduce(_ | _), UIntToOH(completerDev), 0.U)
  val claimCompleteRegFields = Seq.tabulate(nHarts) { i =>
    PLICConstants.claimCompleteBase(i) -> RegMapField(32,
      ReadFn {
        valid =>
          claimer(i) := valid
          (true.B, maxDevs(i))
      }
      , WriteFn {
        (valid, data) =>
          completerDev := data(log2Ceil(nDevices + 1) - 1, 0)
          completer(i) := valid && enable_vec_0(i)(completerDev)
          true.B
      })
  }

  (gateways zip completedDevs.asBools) foreach {
    case (g, c) =>
      g.complete := c
  }

  ((pendingRegs zip gateways) zip claimedDevs) foreach {
    case ((p, g), c) =>
      g.ready := !p
      when (c || g.valid) { p := !c }
  }

  val regMap = priorityRegFields ++ pendingRegFields ++ enableRegFields ++ thresholdRegFields ++ claimCompleteRegFields

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
  val rDECERR = mem_rd_en & !regMap.map(p => (p._1.U === getOffset(read_addr))).reduce(_ | _)
  val rdata = Mux1H(regMap.map(p => (p._1.U === getOffset(read_addr), p._2.readFn.fn(true.B, true.B)._3)))

  //  Write
  val wmask = io_in.w.bits.strb // FillInterleaved(8, io.axi.w.bits.strb)
  regMap.map { case (a, r) =>
    r.writeFn.fn(mem_wr_en, getOffset(write_addr) === a.U, MaskData(r.readFn.fn(true.B, true.B)._3, io_in.w.bits.data, wmask))
  }
  val wDECERR = mem_wr_en & !regMap.map(p => (p._1.U === getOffset(write_addr))).reduce(_ | _)

  //  Write
  b_bits.id := aw_bits_reg.id
  b_bits.user := aw_bits_reg.user
  b_bits.resp := Mux(wDECERR, io_in.params.RESP_DECERR, io_in.params.RESP_OKAY)
  io_in.b.bits := b_bits

  //  Read
  r_bits.id := ar_bits_reg.id
  r_bits.data := rdata
  r_bits.user := ar_bits_reg.user
  r_bits.last := read_cnt === 0.U
  r_bits.resp := Mux(rDECERR, io_in.params.RESP_DECERR, io_in.params.RESP_OKAY)
  io_in.r.bits := r_bits

  //  End
}