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
package zircon.ifu.icache

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket.Causes
import zircon.common._
import zircon.axi4._
import zircon.mmu._
import zircon.csr._
import zircon.util._

class ICacheStageReq(implicit p: Parameters) extends BaseZirconBundle {
  val addr = UInt(vaddrBits.W)
}

class ICacheStageResp(implicit p: Parameters) extends BaseZirconBundle {
  val addr    = UInt(vaddrBits.W)
  val data    = UInt(icacheParams.fetchBits.W)
  val cause   = UInt(eLen.W)
}

class ICacheStageIO(implicit p: Parameters) extends BaseZirconBundle {
  val stall = Input(Bool())

  //  Flush pipeline
  val flush = Input(Bool())

  val satp = Input(new Satp)
  val prv = Input(UInt(2.W))
  val sfence = Flipped(Decoupled(new SFenceReq))

  //  Require
  val req = Flipped(Valid(new ICacheStageReq))
  val resp = Valid(new ICacheStageResp)

  //  PerfCounters
  val icache_access = Output(Bool())
  val icache_miss = Output(Bool())
  val itlb_access = Output(Bool())
  val itlb_miss = Output(Bool())

  //  Forward stall
  val forward_stall = Output(Bool())
}

class ICacheStage(params: Seq[AXI4MasterParams])(implicit p: Parameters) extends BaseAXI4MasterModule(params, new ICacheStageIO)
  with HasZirconCoreParameters
{
  override lazy val module = new AXI4MasterModuleImp[ICacheStageIO](this) {
    val ext_io = io.extend_io.get
    assert(ext_io == None)
    val (out, _) = node.out.head

    val itlb = Module(new ITLB(icacheParams.nTLBSets, icacheParams.nTLBWays))
    val icache = Module(new ICache)

    //  AXI4 FSM
    val s_idle::s_write_req::s_write_data::s_write_wait::s_read_req::s_read_wait::Nil = Enum(6)
    val axi_state = RegInit(s_idle)

    //  ICache FSM
    val s_read::s_itlb_req::s_itlb_wait::s_icache_req::s_icache_wait::s_tag_req::s_invalidate::s_done::Nil = Enum(9)
    val icache_state = RegInit(s_read)

    //  PTW FSM
    val s_ready::s_req::s_wait::s_fragment_superpage::Nil = Enum(4)
    val ptw_state = RegInit(s_ready)

    val s2_vld = Reg(Bool())
    val s2_addr = Reg(UInt(vaddrBits.W))

    //  AXI4  FSM
    switch (axi_state) {
      is (s_idle) {
        when (ptw_state === s_req || (s2_vld && icache.io.resp.miss)) {
          axi_state := s_read_req
        }
      }
      is (s_write_req) {
        when (out.aw.fire) {
          axi_state := s_write_data
        }
      }
      is (s_write_data) {
        when (out.w.fire && out.w.bits.last) {
          axi_state := s_write_wait
        }
      }
      is (s_write_wait) {
        when (out.b.fire) {
          axi_state := s_idle
        }
      }
      is (s_read_req) {
        when (out.ar.fire) {
          axi_state := s_read_wait
        }
      }
      is (s_read_wait) {
        when (out.r.fire && out.r.bits.last) {
          axi_state := s_idle
        }
      }
    }


    //  PTW FSM
    val pg_count = Reg(UInt(lgPgLevels.W))
    val r_pte = Reg(new PTE)
    val (pte, invalid_addr) = {
      val temp = out.r.bits.data(xLen-1, 0).asTypeOf(new PTE)
      val res = WireInit(temp)
      res.ppn := temp.ppn
      when (temp.r || temp.w || temp.x) {
        for (i <- 0 until pgLevels-1) {
          when (pg_count <= i.U && temp.ppn((pgLevels-1-i)*pgLevelBits-1, (pgLevels-2-i)*pgLevelBits) =/= 0.U) {
            res.v := false.B
          }
        }
      }
      (res, (temp.ppn >> ppnBits).asUInt =/= 0.U)
    }
    val superpage_ppn = {
      val choices = VecInit((pgLevels - 1 until 0 by -1).map(i => Cat(r_pte.ppn >> (pgLevelBits * i), s2_addr(pgLevelBits*i-1,0))))
      choices(pg_count)
    }

    def makePTE(x: UInt, d: PTE) = {
      val pte = WireInit(d)
      pte.ppn := x
      pte
    }

    r_pte := Mux(out.r.fire, pte,
              Mux(ptw_state === s_fragment_superpage, makePTE(superpage_ppn, r_pte),
                Mux(icache_state === s_read, makePTE(ext_io.satp.ppn, r_pte), r_pte)))

    val traverse = pte.table && !invalid_addr && pg_count < (pgLevels-1).U
    val pte_addr = {
      val vpn_idxs = VecInit((0 until pgLevels).map(i => (s2_addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0)))
      val vpn_idx = vpn_idxs(pg_count)
      (Cat(r_pte.ppn, vpn_idx) << log2Ceil(xLen/8)).asUInt
    }

    //
    val pte_valid = Wire(Bool())
    val xpt_valid = Wire(Bool())
    val pte_superpage = Reg(Bool())
    pte_valid := false.B
    xpt_valid := false.B

    switch (ptw_state) {
      is (s_ready) {
        when (s2_vld && itlb.io.resp.miss) {
          ptw_state := s_req
        }
        pg_count := (pgLevels-minPgLevels).U
        pte_superpage := false.B
      }
      is (s_req) {
        when (out.ar.fire) {
          ptw_state := s_wait
        }
      }
      is (s_wait) {
        when (xpt_valid) {
          ptw_state := s_ready
        } .elsewhen (traverse) {
          ptw_state := s_req
          pg_count := pg_count + 1.U
        } .otherwise {
          pte_valid := pte.v && !invalid_addr && pg_count === (pgLevels-1).U
          val access_xpt = pte.v && invalid_addr
          when (pg_count =/= (pgLevels-1).U && !access_xpt) {
            ptw_state := s_fragment_superpage
            pg_count := pg_count -  1.U
            pte_superpage := true.B
          } .otherwise {
            ptw_state := s_ready
          }
        }
      }
      is (s_fragment_superpage) {
        ptw_state := s_ready
      }
    }

    //
    //  AW Channel
    out.aw.valid       := axi_state === s_write_req
    out.aw.bits.id     := 0.U
    out.aw.bits.addr   := 0.U
    out.aw.bits.len    := 1.U
    out.aw.bits.size   := out.params.SIZE_16B
    out.aw.bits.burst  := out.params.BURST_INCR
    out.aw.bits.lock   := 0.U
    out.aw.bits.cache  := 0.U
    out.aw.bits.prot   := out.params.PROT_INSECURE
    out.aw.bits.qos    := 0.U
    out.aw.bits.user   := 0.U

    //  W Channel
    out.w.valid     := axi_state === s_write_data
    out.w.bits.data := 0.U
    out.w.bits.strb := 0.U
    out.w.bits.last := true.B

    //  B Channel
    out.b.ready     := axi_state === s_write_wait

    //  AR Channel
    val data_count = Reg(UInt(3.W))
    val rdata = Reg(Vec(4, UInt(out.params.dataBits.W)))
    val itlb_cause = Reg(UInt(eLen.W))
    val icache_cause = Reg(UInt(eLen.W))
    val itlb_hit_data = Mux((icache_state === s_itlb_wait) && out.r.fire, r_pte.ppn, itlb.io.resp.ppn)

    out.ar.valid       := (ptw_state === s_req) || (icache_state === s_icache_req)
    out.ar.bits.id     := 0.U
    out.ar.bits.addr   := Mux(ptw_state === s_req, pte_addr, Cat(itlb_hit_data, s2_addr(pgIdxBits, 0)))
    out.ar.bits.len    := Mux(ptw_state === s_req, 1.U, 4.U)
    out.ar.bits.size   := out.params.SIZE_16B
    out.ar.bits.burst  := out.params.BURST_INCR
    out.ar.bits.lock   := 0.U
    out.ar.bits.cache  := 0.U
    out.ar.bits.prot   := out.params.PROT_INSECURE
    out.ar.bits.qos    := 0.U
    out.ar.bits.user   := 0.U

    //  R Channel
    out.r.ready        := axi_state === s_read_wait
    when (icache_state === s_idle) {
      data_count := 0.U
      icache_cause := 0.U
    } .elsewhen (out.r.fire && (icache_state === s_icache_wait)) {
      rdata(data_count(1,0)) := out.r.bits.data
      when (out.r.bits.resp === out.params.RESP_OKAY) {
        data_count := data_count + 1.U
        icache_cause := 0.U
      } .otherwise {
        icache_cause := Causes.fetch_access.U
      }
    }

    when (icache_state === s_idle) {
      itlb_cause := 0.U
    } .elsewhen (out.r.fire && (icache_state === s_itlb_wait)) {
      itlb_cause := Mux(out.r.bits.resp === out.params.RESP_OKAY, 0.U, Causes.fetch_page_fault.U)
    }

    //  ICache FSM
    val invalid_done = Reg(Bool())
    val invalid_index = Reg(UInt(log2Ceil(icacheParams.nTLBSets).W))

    switch (icache_state) {
      is (s_read) {
        when (ext_io.sfence.valid) {
          icache_state := s_tag_req
        } .elsewhen (s2_vld) {
          icache_state := Mux(itlb.io.resp.miss, s_itlb_req,
                            Mux(icache.io.resp.miss, s_icache_req, s_read))
        }
      }
      is (s_itlb_req) {
        when (out.ar.fire) {
          icache_state := s_itlb_wait
        }
      }
      is (s_itlb_wait) {
        when (out.r.fire) {
          when (out.r.bits.resp =/= out.params.RESP_OKAY) {
            icache_state := s_read
          } .elsewhen (icache.io.resp.miss) {
            icache_state := s_icache_req
          } .otherwise {
            icache_state := s_read
          }
        }
      }
      is (s_icache_req) {
        when (out.ar.fire) {
          icache_state := s_icache_wait
        }
      }
      is (s_icache_wait) {
        when (out.r.fire) {
          icache_state := s_read
        }
      }
      is (s_tag_req) {
        icache_state := s_invalidate
      }
      is (s_invalidate) {
        when (invalid_done) {
          icache_state := s_done
        } .otherwise {
          icache_state := s_tag_req
        }
      }
      is (s_done) {
        icache_state := s_read
      }
    }

    when (ext_io.flush) {
      icache_state := s_read
    } .elsewhen (ext_io.sfence.valid) {
      icache_state := s_tag_req
    }

    //
    val icache_state_is_read = icache_state === s_read
    val icache_state_is_tag_req = icache_state === s_tag_req
    val icache_state_is_invalidate = icache_state === s_invalidate

    when (ext_io.flush || ext_io.sfence.valid) {
      s2_vld := false.B
    } .elsewhen (ext_io.stall) {
      s2_vld := s2_vld
    } .elsewhen (icache_state_is_read) {
      s2_vld := ext_io.req.valid
      s2_addr := ext_io.req.bits.addr
    }

    //
    //  ITLB Require
    //  ITLB Require Valid when
    //  1. Current state is READ
    //  2. Send Tag Read Request to ICache.
    itlb.io.req.valid := (icache_state_is_read && ext_io.req.valid) || icache_state_is_tag_req
    itlb.io.req.bits.addr := Mux(icache_state_is_read, ext_io.req.bits.addr, Cat(0.U, invalid_index, 0.U(6.W)))

    //  ITLB Invalidate
    //  ITLB Invalidate Valid when
    //  1. Do Line Invalid and Clear Valid Bit of Tag.
    itlb.io.sfence.valid          := icache_state_is_invalidate
    itlb.io.sfence.bits.vpn_vld   := ext_io.sfence.bits.vpn_vld
    itlb.io.sfence.bits.vpn       := ext_io.sfence.bits.vpn
    itlb.io.sfence.bits.asid_vld  := ext_io.sfence.bits.asid_vld
    itlb.io.sfence.bits.asid      := ext_io.sfence.bits.asid

    when (icache_state_is_invalidate) {
      invalid_index := invalid_index + 1.U
    }
    invalid_done := (invalid_index === ((1 << log2Ceil(icacheParams.nSets)) - 1).U)
    ext_io.sfence.ready := icache_state === s_done

    //  ITLB to Performance
    ext_io.itlb_access := RegNext(icache_state_is_read & ext_io.req.valid & !ext_io.stall)
    ext_io.itlb_miss := RegNext(icache_state_is_read && itlb.io.resp.miss)

    //  ICache Require
    //  ICache Require Valid when
    //  1. Current State is READ.
    icache.io.req.valid := icache_state_is_read
    icache.io.req.bits.addr := ext_io.req.bits.addr
    icache.io.req.bits.ppn := itlb_hit_data

    //  ICache invalidate
    //  ICache Invalidate Valid when
    //  1. Do Line Invalid and Clear Valid Bit of Tag.
    icache.io.sfence.valid := icache_state_is_invalidate
    icache.io.sfence.bits.invalid_all := !ext_io.sfence.bits.vpn_vld && !ext_io.sfence.bits.asid_vld
    icache.io.invalidate := itlb.io.invalidate

    //  ICache Hit Data
    //  1. MMU resp
    //  2. Bypass
    //  3. Read
    val icache_data_sel = UIntToOH(s2_addr(5,4)).asBools
    val icache_data_vec = (0 until 4).map(i => icache.io.resp.data(128*(i+1)-1, 128*i))
    val icache_bypass_data = Mux1H(icache_data_sel, rdata)

    //  ICache to Performance
    ext_io.icache_access := ext_io.itlb_access
    ext_io.icache_miss := RegNext((icache_state_is_read && !itlb.io.resp.miss && icache.io.resp.miss) ||
      ((icache_state === s_itlb_wait) && out.r.fire && icache.io.resp.miss))

    //  ICache to Scan
    val resp_valid = Reg(Bool())
    val resp_addr = Reg(UInt(vaddrBits.W))
    val resp_data = Reg(UInt(icacheParams.fetchBits.W))
    val resp_cause = Reg(UInt(eLen.W))

    when (ext_io.flush || ext_io.sfence.valid) {
      resp_valid := false.B
    } .elsewhen (ext_io.stall) {
      resp_valid := resp_valid
    } .otherwise {
      resp_valid := s2_vld && icache_state_is_read
    }

    when (ext_io.stall) {
      resp_addr := resp_addr
      resp_data := resp_data
      resp_cause:= resp_cause
    } .otherwise {
      resp_addr := s2_addr
      resp_data := Mux1H(icache_data_sel, icache_data_vec)
      resp_cause:= itlb_cause | icache_cause
    }

    ext_io.resp.valid := resp_valid
    ext_io.resp.bits.addr := resp_addr
    ext_io.resp.bits.data := Mux(RegNext(icache.io.resp.miss), icache_bypass_data, resp_data)
    ext_io.resp.bits.cause := resp_cause

    ext_io.forward_stall := !icache_state_is_read || ext_io.stall
  }
}