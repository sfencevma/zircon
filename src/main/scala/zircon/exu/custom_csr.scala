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
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.FPConstants
import zircon.common._
import zircon.utils._
import difftest._
import scala.collection.mutable._

class CsrReadPortIO(implicit p: Parameters) extends BaseZirconBundle {
  val addr = Input(UInt(csrAddrBits.W))
  val data = Output(UInt(xLen.W))
  val cause = Output(UInt(eLen.W))
}

class CsrWritePortIO(implicit p: Parameters) extends BaseZirconBundle {
  val valid   = Input(Bool())
  val addr    = Input(UInt(csrAddrBits.W))
  val data    = Input(UInt(xLen.W))
  val cause   = Output(UInt(eLen.W))
}

class Interrupts extends Bundle {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
}

class Exceptions(implicit p: Parameters) extends BaseZirconBundle {
  val ecall = Bool()
  val ebreak = Bool()
  val mret = Bool()
  val sret = Bool()
  val uret = Bool()
  val wfi = Bool()
  val xpt = Bool()
  val cause = UInt(eLen.W)
  val addr = UInt(vaddrBits.W)
  val tval = UInt(xLen.W)
}


class PMUUpdate(implicit p: Parameters) extends BaseZirconBundle {
  ////////////////////////////////////////////////////////////////////////////////
  //  The core will support capturing the following events.
  //  Event number    |       Description
  //      1           |   number of misprediction
  //      2           |   number of exceptions
  //      3           |   number of interrupts
  //      4           |   number of csr instructions
  //      5           |   number of jump instructions
  //      6           |   number of branch instructions
  //      7           |   number of mul/div instructions
  //      8           |   number of FPU instructions.
  //      9           |   number of I$ accesses
  //      10          |   number of I$ miss
  //      11          |   number of D$ read accesses
  //      12          |   number of D$ write accesses
  //      13          |   number of D$ atomic accesses
  //      14          |   number of D$ read miss
  //      15          |   number of D$ write miss
  //      16          |   number of D$ atomic miss
  //      17          |   number of pipeline stalls
  //      18          |   number of pipeline flush
  //      19          |   number of ecall
  //      20          |   number of ebreak
  //      21          |   reserved
  //      22          |   reserved
  //      23          |   reserved
  //      24          |   reserved
  //      25          |   reserved
  //      26          |   reserved
  //      27          |   reserved
  //      28          |   reserved
  //      29          |   reserved
  //      30          |   reserved
  ////////////////////////////////////////////////////////////////////////////////
  val mispred = Bool()
  val is_csr = Bool()
  val is_jmp = Bool()
  val is_br = Bool()
  val is_ebreak = Bool()
  val is_ecall = Bool()
  val is_mdus = Vec(retireWidth, Bool())
  val is_fpus = Vec(retireWidth, Bool())
  val itlb_access = Bool()
  val itlb_miss = Bool()
  val icache_access = Bool()
  val icache_miss = Bool()
  val dtlb_access = Bool()
  val dtlb_miss = Bool()
  val dcache_read_access = Bool()
  val dcache_write_access = Bool()
  val dcache_atom_access = Bool()
  val dcache_read_miss = Bool()
  val dcache_write_miss = Bool()
  val dcache_atom_miss = Bool()
  val stall = Bool()
  val flush = Bool()
}

class Misa extends Bundle {
  val mxl = UInt(2.W)
  val zero = UInt(36.W) //  MXLEN-28 => 64-28
  val z = Bool()
  val y = Bool()
  val x = Bool()
  val w = Bool()
  val v = Bool()
  val u = Bool()
  val t = Bool()
  val s = Bool()
  val r = Bool()
  val q = Bool()
  val p = Bool()
  val o = Bool()
  val n = Bool()
  val m = Bool()
  val l = Bool()
  val k = Bool()
  val j = Bool()
  val i = Bool()
  val h = Bool()
  val g = Bool()
  val f = Bool()
  val e = Bool()
  val d = Bool()
  val c = Bool()
  val b = Bool()
  val a = Bool()
}

class MStatus extends Bundle {
  val sd = Bool()
  val zero1 = UInt(23.W)
  val mpv = Bool()
  val gva = Bool()
  val mbe = Bool()
  val sbe = Bool()
  val sxl = UInt(2.W)
  val uxl = UInt(2.W)
  val zero2 = UInt(9.W)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()
  val mxr = Bool()
  val sum = Bool()
  val mprv = Bool()
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val mpp = UInt(2.W)
  val vs = UInt(2.W)
  val spp = Bool()
  val mpie = Bool()
  val ube = Bool()
  val spie = Bool()
  val zero3 = Bool()
  val mie = Bool()
  val zero4 = Bool()
  val sie = Bool()
  val zero5 = Bool()
}

class MIP extends Bundle {
  val zero1 = UInt(51.W)
  val sgeip = Bool()
  val meip = Bool()
  val vseip = Bool()
  val seip = Bool()
  val zero2 = Bool()
  val mtip = Bool()
  val vstip = Bool()
  val stip = Bool()
  val zero3 = Bool()
  val msip = Bool()
  val vssip = Bool()
  val ssip = Bool()
  val zero4 = Bool()
}

class MIE extends Bundle {
  val zero1 = UInt(51.W)
  val sgeie = Bool()
  val meie = Bool()
  val vseie = Bool()
  val seie = Bool()
  val zero2 = Bool()
  val mtie = Bool()
  val vstie = Bool()
  val stie = Bool()
  val zero3 = Bool()
  val msie = Bool()
  val vssie = Bool()
  val ssie = Bool()
  val zero4 = Bool()
}

class PTBR(implicit p: Parameters) extends BaseZirconBundle {
  val mode = UInt(4.W)
  val asid = UInt(asIdBits.W)
  val ppn  = UInt(ppnBits.W)
}


class CustomCSRsIO(implicit p: Parameters) extends BaseZirconBundle {
  //  CSRs Read and Write Port
  val read_port = new CsrReadPortIO
  val write_port = new CsrWritePortIO

  //  Interrupts and Exceptions
  val interrupts = Input(new Interrupts)
  val xpt = Flipped(Valid(new Exceptions))

  //  Instruction Retire
  val ret = Input(Vec(retireWidth, Bool()))

  //  Exception
  val xpt_catch = Output(Bool()) 
  val evec = Output(UInt(vaddrBits.W))

  //  Priority
  val prv = Output(UInt(2.W))

  //  ASID
  val ptbr = Output(new PTBR)

  //  Floating point Rounding Mode
  val frm = Output(UInt(FPConstants.RM_SZ.W))

  //  FS Dirty
  val set_fs_dirty = Input(Bool())

  //  PMU Update Info
  val perf = Input(new PMUUpdate)

  //  CSR Stall
  val stall = Output(Bool())

  //  Issue Read
  val tvm = Output(Bool())

  //  MMU
  val tsr = Output(Bool())
  val sum = Output(Bool())
  val mprv = Output(Bool())

  //  Difftest
  val hart_id = Input(UInt(64.W))
}


class CustomCSRs(implicit p: Parameters) extends BaseZirconModule {
  val io = IO(new CustomCSRsIO())

  def readEPC(x: UInt) = {
    val res = ~((~x).asUInt | 1.U)
    Cat(Fill(xLen-vaddrBits, res(vaddrBits-1)), res)
  }
  def fromEPC(x: UInt) = {
    val res = ~((~x).asUInt | 1.U)
    Cat(Fill(xLen-vaddrBits, res(vaddrBits-1)), res)
  }
  //  -----------------------------------------------------
  //  Machine Level CSRs

  val reset_misa = Wire(new Misa)
  reset_misa := 0.U.asTypeOf(new Misa)
  reset_misa.mxl := 2.U
  reset_misa.a := true.B
  reset_misa.c := true.B
  reset_misa.d := true.B
  reset_misa.f := true.B
  reset_misa.i := true.B
  reset_misa.m := true.B
  reset_misa.s := true.B
  reset_misa.u := true.B
  val reg_misa = RegInit(new Misa, reset_misa)
  val read_misa = reg_misa.asUInt

  val reg_mvendorid = Wire(UInt(xLen.W))
  reg_mvendorid := 0.U
  val read_mvendorid = reg_mvendorid

  val reg_marchid = Wire(UInt(xLen.W))
  reg_marchid := 0.U
  val read_marchid = reg_marchid

  val reg_mhartid = Wire(UInt(xLen.W))
  reg_mhartid := 0.U
  val read_mhartid = reg_mhartid

  val reg_mimpid = Wire(UInt(xLen.W))
  reg_mimpid := 0.U
  val read_mimpid = reg_mimpid

  val reset_mstatus = 0.U.asTypeOf(new MStatus)
  reset_mstatus.mpp := PRV.M.U
  reset_mstatus.sxl := reset_misa.mxl
  reset_mstatus.uxl := reset_misa.mxl
  val reg_mstatus = RegInit(new MStatus, reset_mstatus)
  val read_mstatus = reg_mstatus.asUInt

  val reg_prv = RegInit(UInt(2.W), PRV.M.U)

  val delegable_exceptions = Seq(
    Causes.misaligned_fetch,
    Causes.fetch_page_fault,
    Causes.breakpoint,
    Causes.load_page_fault,
    Causes.store_page_fault,
    Causes.misaligned_load,
    Causes.misaligned_store,
    Causes.illegal_instruction,
    Causes.user_ecall).map(1 << _).sum

  val reg_medeleg = Reg(UInt(xLen.W))
  val read_medeleg = reg_medeleg & delegable_exceptions.U

  val delegate_interrupts = Wire(UInt(xLen.W))
  val reg_mideleg = RegInit(UInt(xLen.W), 0.U)
  val read_mideleg = reg_mideleg & delegate_interrupts

  val reg_mepc = Reg(UInt(xLen.W))
  val read_mepc = readEPC(reg_mepc)

  val reg_mcause = Reg(UInt(xLen.W))
  val read_mcause = reg_mcause

  val reg_mtval = Reg(UInt(xLen.W))
  val read_mtval = reg_mtval

  val reg_mscratch = Reg(UInt(xLen.W))
  val read_mscratch = reg_mscratch

  val reg_mtvec = Reg(UInt(xLen.W))
  val read_mtvec = reg_mtvec

  val reset_mie = Wire(new MIE)
  reset_mie := 0.U.asTypeOf(new MIE)
  val reg_mie = RegInit(new MIE, reset_mie)
  val read_mie = reg_mie.asUInt

  val support_interrupts = Wire(UInt(xLen.W))
  val reset_mip = Wire(new MIP)
  reset_mip := 0.U.asTypeOf(new MIP)
  val reg_mip = RegInit(new MIP, reset_mip)
  val mip = Wire(new MIP)
  val seip = Wire(Bool())
  mip := 0.U.asTypeOf(new MIP)
  mip.meip := reg_mip.meip
  mip.seip := reg_mip.seip | seip
  mip.mtip := reg_mip.mtip
  mip.stip := reg_mip.stip
  mip.ssip := reg_mip.ssip
  val read_mip = mip.asUInt & support_interrupts

  val reg_mcounteren = Reg(UInt(xLen.W))
  val read_mcounteren = reg_mcounteren

  val reg_mcountinhibit = Reg(UInt(32.W))
  val read_mcountinhibit = Cat(0.U(32.W), reg_mcountinhibit)

  support_interrupts := Cat(
    0.U(52.W),
    1.U(1.W),   //  MEIP
    0.U(1.W),
    1.U(1.W),   //  SEIP
    0.U(1.W),
    1.U(1.W),   //  MTIP
    0.U(1.W),
    1.U(1.W),   //  STIP
    0.U(1.W),
    1.U(1.W),   //  MSIP
    0.U(1.W),
    1.U(1.W),   //  SSIP
    0.U(1.W)
  )

  delegate_interrupts := Cat(
    0.U(52.W),
    0.U(1.W),   //  MEIP
    0.U(1.W),
    1.U(1.W),   //  SEIP
    0.U(1.W),
    0.U(1.W),   //  MTIP
    0.U(1.W),
    1.U(1.W),   //  STIP
    0.U(1.W),
    0.U(1.W),   //  MSIP
    0.U(1.W),
    1.U(1.W),   //  SSIP
    0.U(1.W)
  )

  val reg_mhpmevents = Reg(Vec(nPerfCounters, UInt(xLen.W)))
  val reg_mhpmcounters = Reg(Vec(nPerfCounters, UInt(xLen.W)))

  val m_mapping = LinkedHashMap[Int, Bits](
    CSRs.mstatus -> read_mstatus,
    CSRs.misa -> read_misa,
    CSRs.medeleg -> read_mstatus,
    CSRs.mideleg -> read_mideleg,
    CSRs.mie -> read_mie,
    CSRs.mtvec -> read_mtvec,
    CSRs.mscratch -> read_mscratch,
    CSRs.mepc -> read_mepc,
    CSRs.mvendorid -> read_mvendorid,
    CSRs.marchid -> read_marchid,
    CSRs.mimpid -> read_mimpid,
    CSRs.mhartid -> read_mhartid,
    CSRs.mcause -> read_mcause,
    CSRs.mtval -> read_mtval,
    CSRs.mip -> read_mip,
    CSRs.mcounteren -> read_mcounteren,
    0x320 -> read_mcountinhibit
  )

  for (n <- 0 until nPerfCounters) {
    m_mapping += (CSRs.mhpmevent3 + n) -> reg_mhpmevents(n)
  }
  for (n <- 0 until nPerfCounters) {
    m_mapping += (CSRs.mhpmcounter3 + n) -> reg_mhpmcounters(n)
  }

  val m_decoded_addr = m_mapping map { case (k, v) => k -> (io.write_port.addr === k.U) }

  //  Mstatus
  when (io.write_port.valid) {
    val wdata = io.write_port.data
    when (m_decoded_addr(CSRs.mstatus)) {
      val new_mstatus = WireInit(wdata.asTypeOf(new MStatus))
      reg_mstatus.mie := new_mstatus.mie
      reg_mstatus.mpie := new_mstatus.mpie
      reg_mstatus.tsr := new_mstatus.tsr
      reg_mstatus.tw := new_mstatus.tw
      reg_mstatus.tvm := new_mstatus.tvm
      reg_mstatus.mprv := new_mstatus.mprv
      reg_mstatus.mpp := new_mstatus.mpp
      reg_mstatus.sie := new_mstatus.sie
      reg_mstatus.spie := new_mstatus.spie
      reg_mstatus.spp := new_mstatus.spp

      reg_mstatus.fs := new_mstatus.fs
      reg_mstatus.vs := 0.U
      reg_mstatus.xs := 0.U

      reg_mstatus.sd := new_mstatus.vs === 3.U || new_mstatus.fs === 3.U || new_mstatus.xs === 3.U
    }

    when (m_decoded_addr(CSRs.mtvec)) { reg_mtvec := wdata }
    when (m_decoded_addr(CSRs.medeleg)) { reg_medeleg := wdata }
    when (m_decoded_addr(CSRs.mideleg)) { reg_mideleg := wdata }
    when (m_decoded_addr(CSRs.mcounteren)) { reg_mcounteren := wdata }
    when (m_decoded_addr(CSRs.mie)) { reg_mie := (wdata & support_interrupts).asTypeOf(new MIE) }
    when (m_decoded_addr(CSRs.mepc)) { reg_mepc := fromEPC(wdata) }
    when (m_decoded_addr(CSRs.mscratch)) { reg_mscratch := wdata }
    when (m_decoded_addr(CSRs.mtvec)) { reg_mtvec := wdata }
    when (m_decoded_addr(CSRs.mcause)) { reg_mcause := wdata }
    when (m_decoded_addr(CSRs.mtval)) { reg_mtval := wdata }

    for (n <- 0 until nPerfCounters) {
      when (m_decoded_addr(CSRs.mhpmevent3 + n)) { reg_mhpmevents(n) := wdata }
      when (m_decoded_addr(CSRs.mhpmcounter3 + n)) { reg_mhpmcounters(n) := wdata }
    }

    when (m_decoded_addr(CSRs.mcounteren)) { reg_mcounteren := wdata }
    when (m_decoded_addr(0x320)) { reg_mcountinhibit := wdata & ~2.U } //   TM == 0

    when (m_decoded_addr(CSRs.mip)) {
      val new_mip = WireInit(wdata.asTypeOf(new MIP))
      reg_mip.seip := new_mip.seip
      reg_mip.stip := new_mip.stip
      reg_mip.seip := new_mip.seip
    }
  }

  reg_mip.meip := io.interrupts.meip
  reg_mip.mtip := io.interrupts.mtip
  reg_mip.msip := io.interrupts.msip
  io.prv := reg_prv

  //  -----------------------------------------------------
  //  Supervisor Level CSRs
  val reg_sstatus = Wire(new MStatus)
  reg_sstatus := 0.U.asTypeOf(new MStatus)
  reg_sstatus.sd := reg_mstatus.sd
  reg_sstatus.uxl := reg_mstatus.uxl
  reg_sstatus.mxr := reg_mstatus.mxr
  reg_sstatus.sum := reg_mstatus.sum
  reg_sstatus.xs := reg_mstatus.xs
  reg_sstatus.fs := reg_mstatus.fs
  reg_sstatus.vs := reg_mstatus.vs
  reg_sstatus.spp := reg_mstatus.spp
  reg_sstatus.ube := reg_mstatus.ube
  reg_sstatus.spie := reg_mstatus.spie
  reg_sstatus.sie := reg_mstatus.sie
  val read_sstatus = reg_sstatus.asUInt

  val reg_sepc = Reg(UInt(xLen.W))
  val read_sepc = readEPC(reg_sepc)

  val reg_scause = Reg(UInt(xLen.W))
  val read_scause = reg_scause

  val reg_stval = Reg(UInt(xLen.W))
  val read_stval = reg_stval

  val reg_sscratch = Reg(UInt(xLen.W))
  val read_sscratch = reg_sscratch

  val reg_stvec = Reg(UInt(xLen.W))
  val read_stvec = reg_stvec

  val reset_satp = Wire(new PTBR)
  reset_satp := 0.U.asTypeOf(new PTBR)
  val reg_satp = RegInit(new PTBR, reset_satp)
  val read_satp = reg_satp.asUInt

  val reg_wfi = Reg(Bool())
  val read_sip = read_mip & reg_mideleg
  val read_sie = read_mie & reg_mideleg

  val reg_scounteren = Reg(UInt(32.W))
  val read_scounteren = Cat(0.U(32.W), reg_scounteren)

  val s_mapping = LinkedHashMap[Int, Bits] (
    CSRs.sstatus -> read_sstatus,
    CSRs.sie -> read_sie,
    CSRs.stvec -> read_stvec,
    CSRs.sscratch -> read_sscratch,
    CSRs.sepc -> read_sepc,
    CSRs.scause -> read_scause,
    CSRs.stval -> read_stval,
    CSRs.sip -> read_sip,
    CSRs.satp -> read_satp,
    CSRs.scounteren -> read_scounteren
  )

  val s_decoded_addr = s_mapping map { case (k, v) => k -> (io.write_port.addr === k.U) }
  when (io.write_port.valid) {
    val wdata = WireInit(io.write_port.data)
    when (s_decoded_addr(CSRs.sstatus)) {
      val new_sstatus = WireInit(wdata.asTypeOf(new MStatus))
      reg_mstatus.sie := new_sstatus.sie
      reg_mstatus.spie := new_sstatus.spie
      reg_mstatus.spp := new_sstatus.spp
      reg_mstatus.mxr := new_sstatus.mxr
      reg_mstatus.sum := new_sstatus.sum
      reg_mstatus.fs := new_sstatus.fs
      reg_mstatus.vs := 0.U
      reg_mstatus.xs := 0.U
    }
    when (s_decoded_addr(CSRs.sip)) {
      val new_sip = WireInit(wdata & ~read_mideleg).asTypeOf(new MIP)
      reg_mip.ssip := new_sip.ssip
    }
    when (s_decoded_addr(CSRs.sie)) { reg_mie := ((read_mie & ~read_mideleg) | (wdata & read_mideleg)).asTypeOf(new MIE) }
    when (s_decoded_addr(CSRs.satp)) {
      val new_satp = wdata.asTypeOf(new PTBR)
      reg_satp.mode := 9.U
      reg_satp.asid := new_satp.asid
      reg_satp.ppn := new_satp.ppn
    }
    when (s_decoded_addr(CSRs.sscratch)) { reg_sscratch := wdata }
    when (s_decoded_addr(CSRs.sepc)) { reg_sepc := fromEPC(wdata) }
    when (s_decoded_addr(CSRs.stvec)) { reg_stvec := wdata }
    when (s_decoded_addr(CSRs.scause)) { reg_scause := wdata }
    when (s_decoded_addr(CSRs.stval)) { reg_stval := wdata }
    when (s_decoded_addr(CSRs.scounteren)) { reg_scounteren := wdata }
  }

  io.ptbr := reg_satp
  //  ----------------------------------------------------
  //  FPU CSRs
  val reg_fflags = Reg(UInt(FPConstants.FLAGS_SZ.W))
  val reg_frm = Reg(UInt(FPConstants.RM_SZ.W))
  val read_fflags = reg_fflags // Cat(0.U(59.W), reg_fflags)
  val read_frm = reg_frm // Cat(0.U(61.W), reg_frm)
  val read_fcsr = Cat(reg_frm, reg_fflags)
  val set_fs_dirty = WireInit(io.set_fs_dirty)

  val f_mapping = LinkedHashMap[Int, Bits] (
    CSRs.fflags -> read_fflags,
    CSRs.frm -> read_frm,
    CSRs.fcsr -> read_fcsr
  )

  val f_decoded_addr = f_mapping map { case (k, v) => k -> (io.write_port.addr === k.U) }
  when (io.write_port.valid) {
    val wdata = io.write_port.data
    when (f_decoded_addr(CSRs.fflags)) {
      reg_fflags := reg_fflags | wdata
      set_fs_dirty := true.B
    }
    when (f_decoded_addr(CSRs.frm)) {
      reg_frm := wdata
      set_fs_dirty := true.B
    }
    when (f_decoded_addr(CSRs.fcsr)) {
      reg_fflags := wdata
      reg_frm := wdata >> FPConstants.FLAGS_SZ
      set_fs_dirty := true.B
    }
  }


  when (set_fs_dirty) {
    reg_mstatus.fs := 3.U
  }

  io.frm := read_frm

  //  ----------------------------------------------------
  val reg_instret = Reg(UInt(xLen.W))
  val read_instret = reg_instret
  val reg_cycle = Reg(UInt(xLen.W))
  val read_cycle = reg_cycle

  val mm_mapping = LinkedHashMap[Int, Bits] (
    CSRs.instret -> read_instret,
    CSRs.cycle -> read_cycle,
    CSRs.minstret -> read_instret,
    CSRs.mcycle -> read_cycle
  )

  for (n <- 0 until nPerfCounters) {
    mm_mapping += (n + CSRs.hpmcounter3) -> reg_mhpmcounters(n)
  }

  when (io.ret.reduce(_|_) && !reg_mcountinhibit(2)) { reg_instret := reg_instret + PopCount(io.ret) }
  when (reg_mcountinhibit(0)) { reg_cycle := reg_cycle + 1.U }

  val mm_decoded_addr = mm_mapping map { case (k, v) => k -> (io.write_port.addr === k.U) }
  val hpmEvtBits: Int = log2Up(nPerfCounters)
  when (io.write_port.valid) {
    val wdata = io.write_port.data
    when (mm_decoded_addr(CSRs.minstret)) { reg_instret := wdata }
    when (mm_decoded_addr(CSRs.mcycle)) { reg_cycle := wdata }
  }

  //  Read  CSRs
  val read_mapping = m_mapping ++ s_mapping ++ f_mapping ++ mm_mapping
  val read_decoded_addr = read_mapping map { case (k, v) => k -> (io.read_port.addr === k.U) }
  val invalid_addr = !(read_mapping.map { case (k, v) => read_decoded_addr(k) } reduce (_|_))
  io.read_port.data := Mux1H(for ((k, v) <- read_mapping) yield read_decoded_addr(k) -> v)
  io.read_port.cause := Mux(invalid_addr, Causes.illegal_instruction.U, 0.U)

  //  Interrupts
  def chooseInterrupt(maskIn: UInt) = {
    val intrs = Seq(11, 3, 7, 9, 1, 5, 8, 0, 4)
    val intrs_mapping = intrs.map(i => maskIn(i) -> i.U)
    (intrs.map(i => maskIn(i)).reduce(_|_), PriorityMux(intrs_mapping))
  }
  val pending_intrs = (read_mie & read_mip)
  val m_intrs = Mux((reg_prv <= PRV.S.U || reg_mstatus.mie), pending_intrs & ~read_mideleg, 0.U)
  val s_intrs = Mux((reg_prv < PRV.S.U || (reg_mstatus.sie && (reg_prv === PRV.S.U))), pending_intrs & read_mideleg, 0.U)
  val intrs = m_intrs | s_intrs
  val (intr_vld, intr_cause) = chooseInterrupt(intrs)

  //  Exception
  val ecall = io.xpt.bits.ecall
  val ebreak = io.xpt.bits.ebreak
  val xpt = io.xpt.bits.xpt
  val exception = (io.xpt.valid && (ecall || ebreak || xpt)) || intr_vld
  val mret = io.xpt.valid && io.xpt.bits.mret
  val sret = io.xpt.valid && io.xpt.bits.sret
  val cause = Cat(Mux(ecall | ebreak | xpt, 0.U(1.W), intr_vld), zeroExtend(Mux(ecall, reg_prv + Causes.user_ecall.U,
    Mux(ebreak, Causes.breakpoint.U,
      Mux(xpt, io.xpt.bits.cause, intr_cause))), xLen-1))
  val delegate = (reg_prv <= PRV.S.U) && Mux(cause(xLen-1), read_mideleg(cause(eLen-1,0)), read_medeleg(cause(eLen-1,0)))
  val epc = readEPC(io.xpt.bits.addr)
  seip := (reg_prv <= PRV.S.U && io.interrupts.meip && read_mideleg(9))

  when (exception) {
    when (delegate) {
      reg_sepc := epc
      reg_scause := cause
      reg_stval := io.xpt.bits.tval
      reg_mstatus.spie := reg_mstatus.sie
      reg_mstatus.spp := reg_prv
      reg_mstatus.sie := false.B
      reg_prv := PRV.S.U
    } .otherwise {
      reg_mepc := epc
      reg_mcause := cause
      reg_mtval := io.xpt.bits.tval
      reg_mstatus.mpie := reg_mstatus.mie
      reg_mstatus.mpp := reg_prv
      reg_mstatus.mie := false.B
      reg_prv := PRV.M.U
    }
  }

  val tvec = {
    val base = Mux(delegate, read_stvec, read_mtvec)
    val interrupt_offset = cause(log2Ceil(xLen)-1, 0) << 2
    val interrupt_vec = Cat(base >> (log2Ceil(xLen + 2)), interrupt_offset)
    val do_vec = base(0) & cause(cause.getWidth-1) & (cause(cause.getWidth-2, 0) >> log2Ceil(2)).asUInt === 0.U
    Mux(do_vec, interrupt_vec, (base >> 2 << 2).asUInt)
  }
  val xpt_addr = Wire(UInt(vaddrBits.W))
  xpt_addr := tvec //  Set default
  when (sret) {
    reg_mstatus.sie := reg_mstatus.spie
    reg_mstatus.spie := true.B
    reg_mstatus.spp := PRV.U.U
    reg_prv := reg_mstatus.spp
    xpt_addr := readEPC(read_sepc)
  } .elsewhen (mret) {
    reg_mstatus.mie := reg_mstatus.mpie
    reg_mstatus.mpie := true.B
    reg_mstatus.mpp := PRV.U.U
    reg_prv := reg_mstatus.mpp
    xpt_addr := readEPC(read_mepc)
  }

  io.xpt_catch := sret | mret | exception 
  io.evec := RegNext(xpt_addr)

  //  Update HPMC
  val imp_mhpevts = Seq (
    false.B,
    io.perf.mispred,
    exception,
    intr_vld,
    io.perf.is_csr,
    io.perf.is_jmp,
    io.perf.is_br,
    io.perf.is_ebreak,
    io.perf.is_ecall,
    io.perf.is_mdus.reduce(_|_),
    io.perf.is_fpus.reduce(_|_),
    io.perf.icache_access,
    io.perf.icache_miss,
    io.perf.itlb_access,
    io.perf.itlb_miss,
    io.perf.dcache_read_access,
    io.perf.dcache_write_access,
    io.perf.dcache_atom_access,
    io.perf.dcache_read_miss,
    io.perf.dcache_write_miss,
    io.perf.dcache_atom_miss,
    io.perf.dtlb_access,
    io.perf.dtlb_miss,
    io.perf.stall,
    io.perf.flush,
    ecall,
    ebreak
  )
  val support_mhpevts = VecInit(imp_mhpevts ++ Seq.fill(xLen-imp_mhpevts.size) { false.B })

  for (n <- 0 until nPerfCounters) {
    val which_event = reg_mhpmevents(n)(hpmEvtBits-1,0)
    val hpmevt_ena = !reg_mcountinhibit(which_event) && !which_event.orR
    when (hpmevt_ena && support_mhpevts(which_event)) {
      reg_mhpmcounters(which_event) := reg_mhpmcounters(which_event) + Mux(which_event === 9.U, PopCount(io.perf.is_mdus),
        Mux(which_event === 10.U, PopCount(io.perf.is_fpus), 1.U))
    }
  }

  //  Write
  val write_decoded_addr = read_mapping map { case (k, v) => k -> (io.write_port.addr === k.U) }
  val write_invalid_addr = !(read_mapping.map { case (k, v) => read_decoded_addr(k) } reduce (_|_))
  io.write_port.cause := RegNext(Mux(write_invalid_addr, Causes.illegal_instruction.U, 0.U))   //  Write fail

  //  WFI
  val wfi_set = io.xpt.valid & io.xpt.bits.wfi
  val wfi_clr = exception | pending_intrs.orR
  val wfi_ena = wfi_set | wfi_clr
  val wfi_nxt = wfi_set | ~wfi_clr
  when (wfi_ena) { reg_wfi := wfi_nxt }

  io.stall := reg_wfi
  io.tvm := reg_mstatus.tvm
  io.tsr := reg_mstatus.tsr
  io.sum := reg_mstatus.sum
  io.mprv := reg_mstatus.mprv

  //  Difftest
  if (env.EnableDifftest) {
    val difftest = Module(new DifftestTrapEvent)
    difftest.io.clock   := clock
    difftest.io.coreid  := io.hart_id
    difftest.io.valid   := RegNext(io.xpt.valid)
    difftest.io.code    := RegNext(io.xpt.bits.cause)
    difftest.io.pc      := RegNext(io.xpt.bits.addr)
    difftest.io.cycleCnt:= read_cycle
    difftest.io.instrCnt:= read_instret
    difftest.io.hasWFI  := reg_wfi
  }


  if (env.EnableDifftest) {
    val difftest = Module(new DifftestArchEvent)
    difftest.io.clock := clock
    difftest.io.coreid := io.hart_id
    difftest.io.intrNO := RegNext(Mux(intr_vld & cause(xLen-1), cause(xLen-2, 0), 0.U))
    difftest.io.cause  := RegNext(Mux(io.xpt.valid & !cause(xLen-1), cause(xLen-2, 0), 0.U))
    difftest.io.exceptionPC := RegNext(io.evec)
    difftest.io.exceptionInst := 0.U
  }
  if (env.EnableDifftest) {
    val difftest = Module(new DifftestCSRState)
    difftest.io.clock     := clock
    difftest.io.coreid    := io.hart_id
    difftest.io.priviledgeMode := reg_prv 
    difftest.io.mstatus   := read_mstatus
    difftest.io.sstatus   := read_sstatus
    difftest.io.mepc      := read_mepc
    difftest.io.sepc      := read_sepc 
    difftest.io.mtval     := read_mtval
    difftest.io.stval     := read_stval 
    difftest.io.mcause    := read_mcause 
    difftest.io.scause    := read_scause
    difftest.io.satp      := read_satp
    difftest.io.mip       := read_mip 
    difftest.io.mie       := read_mie 
    difftest.io.mscratch  := read_mscratch 
    difftest.io.sscratch  := read_sscratch
    difftest.io.mideleg   := read_mideleg
    difftest.io.medeleg   := read_medeleg
  } 

  //  CSRs read & Write Exception
}