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
package zircon.ifu

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import zircon.common._
import zircon.ifu.icache._
import zircon.mmu._
import zircon.csr._
import zircon.axi4._

class IFUStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val addr      = UInt(vaddrBits.W)
  val data      = UInt(icacheParams.fetchBits.W)
  val cause     = UInt(eLen.W)
  val predinfo  = new NextLinePredInfo
}

class IFUStageIO(implicit p: Parameters) extends BaseZirconBundle {
  val stall = Input(Bool())

  //
  val satp = Input(new Satp)
  val prv = Input(UInt(2.W))

  //  Redirection
  val flush = Flipped(Valid(new PCGenRediret))
  val bpd_rdt = Flipped(Valid(new PCGenRediret))

  //  SFence
  val sfence = Flipped(Decoupled(new SFenceReq))

  //  BPD Update
  val bpd_upd = Flipped(Valid(new NextLinePredictorUpdate))

  //  Response
  val resp = Valid(new IFUStageResp)

  //
  val icache_access = Output(Bool())
  val icache_miss = Output(Bool())
  val itlb_access = Output(Bool())
  val itlb_miss = Output(Bool())
}

class IFUStage(params: Seq[AXI4MasterParams])(implicit p: Parameters) extends BaseAXI4MasterModule(params, new IFUStageIO) {
  override lazy val module = new AXI4MasterModuleImp[IFUStageIO](this) {
    val ext_io = io.extend_io.get

    val pcgen = Module(new PCGen)
    val nlp = Module(new NextLinePredictor)
    val icache = LazyModule(new ICacheStage(params)).module
    val icache_io = icache.io.extend_io.get

    //  PCGen
    pcgen.io.stall    := icache_io.forward_stall
    pcgen.io.flush    := ext_io.flush
    pcgen.io.nlp_rdt  := nlp.io.rdt
    pcgen.io.bpd_rdt  := ext_io.bpd_rdt

    //  NLP
    nlp.io.flush  := ext_io.flush
    nlp.io.stall  := icache_io.forward_stall
    nlp.io.req    := pcgen.io.resp
    nlp.io.upd    := ext_io.bpd_upd

    //  ICache
    icache_io.stall       := ext_io.stall
    icache_io.flush       := ext_io.flush
    icache_io.sfence      := ext_io.sfence
    icache_io.satp        := ext_io.satp
    icache_io.prv         := ext_io.prv
    icache_io.req         := pcgen.io.resp
    ext_io.icache_access  := icache_io.icache_access
    ext_io.icache_miss    := icache_io.icache_miss
    ext_io.itlb_access    := icache_io.itlb_access
    ext_io.itlb_miss      := icache_io.itlb_miss

    //  Response
    ext_io.resp.valid         := icache_io.resp.valid
    ext_io.resp.bits.addr     := icache_io.resp.bits.addr
    ext_io.resp.bits.data     := icache_io.resp.bits.data
    ext_io.resp.bits.cause    := icache_io.resp.bits.cause
    ext_io.resp.bits.predinfo := RegEnable(nlp.io.resp.predinfo, !icache_io.forward_stall)

    val (out, _) = node.out.head
    out <> icache.out
  }
}