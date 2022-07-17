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
import zircon.common._
import zircon.util._
import scala.math._

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

  val priorityBase = 0x0
  val pendingBase = 0x1000
  val enableBase = 0x2000
  val thresholdBase = 0x200000
  val claimCompleteBase = 0x200004


  def enableOffset(i: Int) = i * 0x80
  def enableBase(i: Int, w: Int): Int = enableBase + enableOffset(i) + 4 * w
  def thresholdOffset(i: Int): Int = i * 0x1000
  def thresholdBase(i: Int): Int = thresholdBase + thresholdOffset(i)
  def claimCompleteOffset(i: Int): Int = i * 0x1000
  def claimCompleteBase(i: Int): Int = claimCompleteBase + claimCompleteOffset(i)
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

class PLICExtendIO extends Bundle {
  val wdogcmp = Vec(PLICConstants.nWdogs, Flipped(new GatewaysIO))
  val rtccmp  = Vec(PLICConstants.nRtcs, Flipped(new GatewaysIO))
  val uart    = Vec(PLICConstants.nUarts, Flipped(new GatewaysIO))
  val qspi    = Vec(PLICConstants.nQspis, Flipped(new GatewaysIO))
  val gpio    = Vec(PLICConstants.nGpios, Flipped(new GatewaysIO))
  val pwm     = Vec(PLICConstants.nPwms, Flipped(new GatewaysIO))
}

class AXI4Plic(params: AXI4SlaveParams, extendIO: PLICExtendIO)(implicit p: Parameters) extends BaseAXI4SlaveModule(params, extendIO) with HasZirconCoreParameters{
  override lazy val module = new AXI4SlaveModuleImp[PLICExtendIO](this) {
    def nDevices = PLICConstants.nDevices
    def nHarts = PLICConstants.nHarts
    def minPriorities = nDevices max 7
    def nPriorities = (1 << log2Ceil(minPriorities + 1)) - 1
    def addressSpaceSize = 0x4000000
    def addressBits = log2Up(addressSpaceSize)
    def getOffset(addr: UInt) = addr(addressBits - 1, 0)

    val ext_io = io.extend_io.get
    val interrupts = ext_io.wdogcmp ++ ext_io.rtccmp ++ ext_io.uart ++
      ext_io.qspi ++ ext_io.gpio ++ ext_io.pwm
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

    val priority = Reg(Vec(nDevices, UInt(32.W)))
    val priorityRegFields = priority.zipWithIndex.map { case (r, i) =>
      PLICConstants.priorityBase + 4 * (i + 1) -> priorityRegField(r)
    }
    val devs = (nDevices + 31) / 32
    val pending = Reg(Vec(devs, UInt(32.W)))
    val pendingRegFields = pending.zipWithIndex.map { case (r, i) =>
      PLICConstants.pendingBase + 4 * i -> RegMapField.r(32, r)
    }
    val enableRegs = Seq.fill(devs)(Reg(UInt(32.W)))
    val enables = Seq.fill(nHarts) {
      enableRegs
    }
    val enable_vec = VecInit(enables.map(s => Cat(s)))
    val enable_vec_0 = VecInit(enable_vec.map(x => Cat(x, 0.U(1.W))))
    val enableRegFields = enables.zipWithIndex.map { case (f, hart) =>
      f.zipWithIndex.map { case (r, w) => PLICConstants.enableBase(hart, w) -> RegMapField(32, r) }
    }.reduce(_ ++ _).toMap
    val threahold = Reg(Vec(nHarts, UInt(32.W)))
    val thresholdRegFields = Seq.tabulate(nHarts) { i =>
      PLICConstants.thresholdBase(i) -> thresholdRegField(threahold(i))
    }
    val harts = Reg(Vec(nHarts, Bool()))
    val maxDevs = Wire(Vec(nHarts, UInt(log2Up(nDevices).W)))
    val pending_vec = Reverse(Cat(pending))
    maxDevs.zipWithIndex.map { case (r, hart) =>
      val taken_vec = pending_vec & Reverse(Cat(enables(hart)))
      r := Mux(taken_vec === 0.U, 0.U, PriorityEncoder(taken_vec))
    }
    val claimer = Wire(Vec(nHarts, Bool()))
    val claimming = Seq.tabulate(nHarts) { i => Mux(claimer(i), maxDevs(i), 0.U) }.reduce(_|_)
    val claimedDevs = VecInit(UIntToOH(claimming).asBools)
    val completer = Wire(Vec(nHarts, Bool()))
    val completerDev = Wire(UInt(log2Up(nDevices).W))
    val completedDevs = Mux(completer.reduce(_|_), UIntToOH(completerDev), 0.U)
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

    (gateways zip completedDevs.asBools().tail) foreach {
      case (g, c) =>
        g.complete := c
    }
    ((pending zip gateways) zip claimedDevs.tail) foreach {
      case ((p, g), c) =>
        g.ready := !p
        when(c || g.valid) {
          p := !c
        }
    }
    val regMap = priorityRegFields ++ pendingRegFields ++ enableRegFields ++ thresholdRegFields ++ claimCompleteRegFields

    interrupts.map(i => i.valid).zipWithIndex.map { case (intr, i) =>
      val id = i + 1
      when(intr) {
        pending(id / 32) := pending(id / 32).bitSet((id % 32).U, true.B)
      }
    }

    //  Read
    val rDECERR = mem_rd_en & !regMap.map(p => (p._1.U === getOffset(read_addr))).reduce(_ | _)
    val rdata = Mux1H(regMap.map(p => (p._1.U === getOffset(read_addr), p._2.readFn.fn(true.B, true.B)._3)))

    //  Write
    val wmask =  in.w.bits.strb.asBools // FillInterleaved(8, io.axi.w.bits.strb)
    regMap.map { case (a, r) =>
      r.writeFn.fn(mem_wr_en, getOffset(write_addr) === a.U, MaskData(r.readFn.fn(true.B, true.B)._3, in.w.bits.data, wmask))
    }
    val wDECERR = mem_wr_en & !regMap.map(p => (p._1.U === getOffset(write_addr))).reduce(_ | _)

    //  Write
    b_bits.id   := aw_bits_reg.id
    b_bits.resp := Mux(wDECERR, in.params.RESP_DECERR, in.params.RESP_OKAY)

    //  Read
    r_bits.id   := ar_bits_reg.id
    r_bits.data := rdata
    r_bits.last := read_cnt === 0.U
    r_bits.resp := Mux(rDECERR, in.params.RESP_DECERR, in.params.RESP_OKAY)
  }
}