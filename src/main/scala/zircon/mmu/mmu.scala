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
package zircon.mmu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.Causes
import zircon.axi4._
import zircon.common._
import zircon.frontend._
import zircon.exu._
import zircon.utils._

class MemReq(dataBits: Int)(implicit p: Parameters) extends BaseZirconBundle with MemoryOpConstants {
  val addr = UInt(paddrBits.W) 
  val cmd  = UInt(M_SZ.W)

  //  TODO: For AXI4
  val len  = UInt(memBusParams.lenBits.W)
  val size = UInt(memBusParams.sizeBits.W)
  val data = UInt(dataBits.W)
}

class MemResp(dataBits: Int)(implicit p: Parameters) extends BaseZirconBundle {
  val data  = UInt(dataBits.W)
  val cause = UInt(eLen.W)
}

class MMU(implicit p: Parameters) extends BaseZirconModule with MemoryOpConstants {
  val io = IO(new Bundle() {
    val hart_id = Input(UInt(64.W))

    val kill = Input(Bool())
    val ptbr = Input(new PTBR)
    val prv = Input(Bool())

    val tsr = Input(Bool())
    val sum = Input(Bool())
    val mprv = Input(Bool())

    //  ICache fetch
    val itlb_ptw = Flipped(new ITLBPTW)
    // val itlb_req    = Flipped(Decoupled(new ITLBPTWReq))
    // val itlb_resp   = Decoupled(new ITLBPTWResp)
    val icache_mem = Flipped(new ICacheMem)

    //  DCache Load
    val dtlb_ptw = Flipped(new DTLBPTW)
    // val dtlb_req    = Flipped(Decoupled(new DTLBPTWReq))
    // val dtlb_resp   = Decoupled(new DTLBPTWResp)
    val dcache_mem = Flipped(new DCacheMem)

    //  MMIO
    val mmio = Flipped(new LSUMMIO)

    //
    val mem_node = AXI4MasterBundle(memBusParams)
    val mmap_node = AXI4MasterBundle(mmapBusParams)
  })

  val mem_io = io.mem_node

  //  ********************************************
  //  PTW
  val ptw = Module(new PTW)

  //  ********************************************
  //  MMIO
  val mmio = Module(new MMIO())

  ptw.io.kill := io.kill
  ptw.io.ptbr := io.ptbr
  ptw.io.prv  := io.prv
  ptw.io.tsr  := io.tsr
  ptw.io.sum  := io.sum
  ptw.io.mprv := io.mprv

  //  ITLB PTW
  ptw.io.itlb_req <> io.itlb_ptw.req
  io.itlb_ptw.resp <> ptw.io.itlb_resp

  //  DTLB PTW
  ptw.io.dtlb_req <> io.dtlb_ptw.req
  io.dtlb_ptw.resp <> ptw.io.dtlb_resp

  //  MMIO
  mmio.io.req  <> io.mmio.req
  io.mmio.resp <> mmio.io.resp
  io.mmap_node <> mmio.io.node

  //  MEM
  //  State description:
  //  s_ready         : Ready!
  //  s_write_req     : Write Require.
  //  s_write_data    : Write Data.
  //  s_write_wait    : Wait for write response.
  //  s_read_req      : Read Require.
  //  s_read_wait     : Wait for read response.
  //  s_done          : Transfer success.
  val s_ready :: s_write_req :: s_write_data :: s_write_wait :: s_read_req :: s_read_wait :: s_done :: Nil = Enum(7)
  val state = RegInit(s_ready)


  val arb = Module(new RRArbiter(new MemReq(dcacheParams.blockBits), 3))
  arb.io.in(0) <> ptw.io.mem.req
  arb.io.in(1) <> io.icache_mem.req 
  arb.io.in(2) <> io.dcache_mem.req
  arb.io.out.ready := state === s_ready

  val chosen = Reg(UInt())
  val choose_ch0 = chosen === 0.U
  val choose_ch1 = chosen === 1.U
  val choose_ch2 = chosen === 2.U
  val request = Reg(new MemReq(dcacheParams.blockBits))
  when (arb.io.out.fire) {
    chosen := arb.io.chosen
    request := arb.io.out.bits
  }

  //
  val master_ready = (choose_ch0 && ptw.io.mem.resp.ready) || 
                     (choose_ch1 && io.icache_mem.resp.ready) || 
                     (choose_ch2 && io.dcache_mem.resp.ready)
  val slave_valid = state === s_done

  //  Write Channel 
  //  Write Data
  val wdata = VecInit((0 until dcacheParams.nBeats).map(i => request.data(128 * (i+1)-1, 128 * i)))
  val count = RegInit(UInt(), 0.U)
  val max_count = Reg(UInt())

  //  Read Data
  val rdata_vec = Reg(Vec(dcacheParams.nBeats, UInt()))
  val rdata = Cat(rdata_vec.reverse)
  val cause = Reg(UInt(eLen.W))

  when (arb.io.out.fire) {
    max_count := request.len - 1.U
    count := 0.U
    cause := 0.U
  } 
  when (mem_io.w.fire || mem_io.r.fire) {
    rdata_vec(count) := mem_io.r.bits.data 
    count := count + 1.U
  }
  when (mem_io.b.fire) {
    cause := Mux(mem_io.b.bits.resp =/= mem_io.params.RESP_OKAY, Causes.store_access.U, 0.U)
  } .elsewhen (mem_io.r.fire) {
    cause := Mux(mem_io.r.bits.resp =/= mem_io.params.RESP_OKAY, Causes.load_access.U, 0.U)
  }


  //  Command channel
  mem_io.aw.valid       := state === s_write_req 
  mem_io.aw.bits        := 0.U.asTypeOf(new AXI4BundleAW(memBusParams))
  mem_io.aw.bits.addr   := request.addr 
  mem_io.aw.bits.len    := request.len 
  mem_io.aw.bits.size   := mem_io.params.SIZE_16B
  mem_io.aw.bits.burst  := mem_io.params.BURST_INCR

  //  Control channel
  mem_io.w.valid        := state === s_write_data
  mem_io.w.bits.data    := wdata(count)
  mem_io.w.bits.strb    := Fill(memBusParams.dataBits/8, 1.U)
  mem_io.w.bits.last    := count === max_count

  //  Response channel
  mem_io.b.ready        := state === s_write_wait && master_ready 

  //  Read Channel 
  //  Command channel
  mem_io.ar.valid       := state === s_read_req 
  mem_io.ar.bits        := 0.U.asTypeOf(new AXI4BundleAR(memBusParams))
  mem_io.ar.bits.addr   := request.addr 
  mem_io.ar.bits.len    := request.len 
  mem_io.ar.bits.size   := mem_io.params.SIZE_16B
  mem_io.ar.bits.burst  := mem_io.params.BURST_INCR

  //  Control channel
  mem_io.r.ready        := state === s_read_wait && master_ready

  //  PTW Response
  ptw.io.mem.resp.valid := choose_ch0 && slave_valid 
  ptw.io.mem.resp.bits.data := rdata
  ptw.io.mem.resp.bits.cause := cause

  //  ICache Response
  io.icache_mem.resp.valid := choose_ch1 && slave_valid 
  io.icache_mem.resp.bits.data := rdata
  io.icache_mem.resp.bits.cause := cause

  //  DCache Response
  io.dcache_mem.resp.valid := choose_ch2 && slave_valid  
  io.dcache_mem.resp.bits.data := rdata
  io.dcache_mem.resp.bits.cause := cause

  //  
  val transfer_done = (choose_ch0 && ptw.io.mem.resp.fire) ||
                      (choose_ch1 && io.icache_mem.resp.fire) || 
                      (choose_ch2 && io.dcache_mem.resp.fire)
  switch (state) {
    is (s_ready) {
      when (arb.io.out.fire) { 
        state := Mux(arb.io.out.bits.cmd === M_XRD, s_read_req, s_write_req) 
      }
    }

    //  Write Channel
    is (s_write_req) {
      when (mem_io.aw.fire) {
        state := s_write_data
      }
    }
    is (s_write_data) {
      when (mem_io.w.fire && mem_io.w.bits.last) {
        state := s_write_wait 
      }
    }
    is (s_write_wait) {
      when (mem_io.b.fire) {
        state := s_done
      }
    }

    //  Read Channel 
    is (s_read_req) {
      when (mem_io.ar.fire) {
        state := s_read_wait
      }
    }
    is (s_read_wait) {
      when (mem_io.r.fire && mem_io.r.bits.last) {
        state := s_done
      }
    }

    //  Transfer success
    is (s_done) {
      when (transfer_done) { state := s_ready }
    }
  }

  //  End
}